module Move.Translations.Utils
  ( unitType,
    booleanType,
    numericType,
    addressType,
    VariableAnnotations (..),
    Scope,
    cpsIdentifier,
    isIdentifierInScope,
    getNacFromScopes,
    extractVariablesFromBindings,
    extractVariablesFromSingleBind,
    annotateBindingsWithUUID,
    inferExprType,
    tryIO,
    utilsLibAddress,
    isTypeParametric,
    iterateWithOthers,
    unionSet
  )
where

import Control.Exception (ErrorCall, Exception (displayException), try)
import Control.Monad.State
  ( MonadState (get, put),
    State,
  )
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Move.AST

-- | Unity type ()
unitType :: Type
unitType = TypeTuple []

-- | bool type
booleanType :: Type
booleanType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") []

-- | Numeric type.
-- For simplicity, it represents the "bigger" of the numeric types in Move, `u256`
numericType :: Type
numericType = TypeConstructor (LocalNameAccessChain $ Identifier "u256") []

-- | Address type
addressType :: Type
addressType = TypeConstructor (LocalNameAccessChain $ Identifier "address") []

-- |
-- Annotations such as UUID and Type assigned to each identifier in the scope
data VariableAnnotations = VariableAnnotations (Maybe AnnotatedUUID) Type
  deriving (Eq, Show)

-- | Represents a local scope
type Scope = Map.Map NameAccessChain VariableAnnotations

-- |
-- The identifier to use for the whole CPS state,
-- that includes both scopes and global storage
cpsIdentifier :: Identifier
cpsIdentifier = Identifier "cps_state"

-- |
-- The named address representing the folder where the Aiken stdlib can be found
utilsLibAddress :: Address
utilsLibAddress = NamedAddress $ Identifier "utils"

-- |
-- Checks if the identifier is present in any of the input scopes
isIdentifierInScope :: Identifier -> [Scope] -> Bool
isIdentifierInScope _ [] = False
isIdentifierInScope ident (x : xs) = Map.member (LocalNameAccessChain ident) x || isIdentifierInScope ident xs

-- |
-- Returns the UUID and inferred type of an identifier present in the scope.
--
-- Throws an error if the identifier is not present in the scope
getNacFromScopes :: NameAccessChain -> [Scope] -> VariableAnnotations
getNacFromScopes nac [] = error $ "Name access chain not found on the scope: " ++ show nac
getNacFromScopes nac (x : xs) = case Map.lookup nac x of
  -- The identifier is not in this scope, proceed recursively
  Nothing -> getNacFromScopes nac xs
  -- The identifier is in this scope, return its UUID and type (if available)
  Just res -> res

-- |
-- Given a binding, returns a list with every binded identifier along with its type, if it could be inferred
extractVariablesFromBindings :: Bindings -> [Scope] -> [(Identifier, VariableAnnotations)]
--  For a single bind, infer its type
extractVariablesFromBindings (Bindings {bindings = BindedSingle bind, bindingsBindType, bindingsBindExpr}) scopes = extractVariablesFromSingleBind bind bindingsBindType bindingsBindExpr scopes
--  For a tuple bind, if neither types nor right value is specified, types can not be inferred
extractVariablesFromBindings (Bindings {bindings = BindedTuple binds, bindingsBindType = Nothing, bindingsBindExpr = Nothing}) scopes =
  concatMap (\bind -> extractVariablesFromSingleBind bind Nothing Nothing scopes) binds
--  For a tuple bind, if types are specified, they must be a tuple of types. No need to look for the right value
extractVariablesFromBindings (Bindings {bindings = BindedTuple binds, bindingsBindType = Just (TypeTuple types)}) scopes =
  if length binds /= length types
    then error ("When inferring binding types: different tuple arity. " ++ show binds ++ ", " ++ show types)
    else concatMap (\(bind, t) -> extractVariablesFromSingleBind bind (Just t) Nothing scopes) (zip binds types)
--  If instead types are not specified, they must be inferred from the right value
extractVariablesFromBindings (Bindings {bindings = BindedTuple binds, bindingsBindType = Nothing, bindingsBindExpr = Just bindingsBindExpr}) scopes =
  case inferExprType bindingsBindExpr scopes of
    TypeUnknown -> concatMap (\bind -> extractVariablesFromSingleBind bind (Just TypeUnknown) Nothing scopes) binds
    -- The right value should resolve to a tuple type with same arity
    TypeTuple types ->
      if length binds /= length types
        then error ("When inferring binding types: different tuple arity. " ++ show binds ++ ", " ++ show types)
        else concatMap (\(bind, t) -> extractVariablesFromSingleBind bind (Just t) Nothing scopes) (zip binds types)
    _ -> error "Can not pattern match a non-tuple value against a tuple"
--  It is not possible to specify a non-tuple type for a tuple binding
extractVariablesFromBindings (Bindings {bindings = BindedTuple _, bindingsBindType = Just _}) _ = error "Found a tuple binding typed with a non-tuple type"

-- |
-- Give a single bind and its corresponding type or expression, tries to infer the type of all the binded identifiers
extractVariablesFromSingleBind :: Bind -> Maybe Type -> Maybe Expr -> [Scope] -> [(Identifier, VariableAnnotations)]
extractVariablesFromSingleBind bind maybeType maybeExpr scopes =
  case bind of
    (BindIdentifier ident maybeUUID) ->
      let inferredType :: Type = case (maybeType, maybeExpr, scopes) of
            --  It is not possible to infer the type of an identifier alone (or in another way, it can be any type)
            (Nothing, Nothing, _) -> TypeUnknown
            --  Identifier with type annotation
            (Just t, _, _) -> t
            -- When an expression is provided, try to infer it
            (Nothing, Just expr, _) -> inferExprType expr scopes
       in [(ident, VariableAnnotations maybeUUID inferredType)]
    (BindNamedStruct (BindedNamedStruct {bnsFields = BindedFields {bindedFields}})) -> concatMap getBindIdentifiersHelper bindedFields
    (BindPositionalStruct (BindedPositionalStruct {bpsFields = BindedFields {bindedFields}})) -> concatMap getBindIdentifiersHelper bindedFields
  where
    getBindIdentifiersHelper :: BindedField -> [(Identifier, VariableAnnotations)]
    -- TODO: Type of the field can be inferred by looking at the type definition (and expression in case of generic type)
    getBindIdentifiersHelper (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing, bindedFieldUUID}) = [(bindFieldIdentifier, VariableAnnotations bindedFieldUUID TypeUnknown)]
    -- TODO: Can be refined by passing the sub-expression to the recursive call
    getBindIdentifiersHelper (BindedField {bindFieldInnerBind = Just innerBind}) = extractVariablesFromSingleBind innerBind Nothing Nothing scopes

-- |
-- Given an AST, adds a unique identifier to all bindings
--
-- Note that existing UUIDs will be overwritten
annotateBindingsWithUUID :: Root -> State AnnotatedUUID Root
annotateBindingsWithUUID root = do
  -- First, handle both a Module and a Script
  case root of
    RModule rModule@Module {moduleTopLevels} -> do
      mappedTopLevels <- handleTopLevels moduleTopLevels
      return $ RModule $ rModule {moduleTopLevels = mappedTopLevels}
    RScript rScript@Script {scriptTopLevels} -> do
      mappedTopLevels <- handleTopLevels scriptTopLevels
      return $ RScript $ rScript {scriptTopLevels = mappedTopLevels}
  where
    -- Each top level function should be annotated both in its parameters and function body
    handleTopLevels :: [TopLevel] -> State AnnotatedUUID [TopLevel]
    handleTopLevels topLevels = do mapM topLevelAnnotator topLevels

    -- Annotates any top level
    topLevelAnnotator :: TopLevel -> State AnnotatedUUID TopLevel
    topLevelAnnotator (TopLevelFunction topLFunction@Function {functionParameters, functionBody}) = do
      -- Annotate the function itself
      functionUUID <- get
      put $ functionUUID + 1

      -- Annotate the parameters of the function
      annotatedParameters <-
        mapM
          ( \param -> do
              curr <- get
              put $ curr + 1
              return $ param {parameterUUID = Just curr}
          )
          functionParameters
      -- Annotate the body with the helper function
      annotatedBody <- mapM (transformBiM functionBodyAnnotator) functionBody
      -- Return the annotated function
      return $
        TopLevelFunction $
          topLFunction
            { functionParameters = annotatedParameters,
              functionBody = annotatedBody,
              functionUUID = Just functionUUID
            }
    -- Also, annotate module constants
    topLevelAnnotator (TopLevelConstant c@Constant {}) = do
      constantUUID' <- get
      put $ constantUUID' + 1
      return $ TopLevelConstant c {constantUUID = Just constantUUID'}
    -- Every other top level node is ignored
    topLevelAnnotator otherTL = return otherTL

    -- Annotates all the bindings in the function body
    functionBodyAnnotator :: Bind -> State AnnotatedUUID Bind
    -- Single bind
    functionBodyAnnotator (BindIdentifier ident _) = do
      curr <- get
      put $ curr + 1
      return $ BindIdentifier ident (Just curr)
    functionBodyAnnotator other = case other of
      -- Handle pattern matching on named structs
      BindNamedStruct str@BindedNamedStruct {bnsFields} -> do
        mappedFields <- bindedFieldsAnnotator bnsFields
        return $ BindNamedStruct $ str {bnsFields = mappedFields}
      -- Handle pattern matching on positional structs
      BindPositionalStruct str@BindedPositionalStruct {bpsFields} -> do
        mappedFields <- bindedFieldsAnnotator bpsFields
        return $ BindPositionalStruct $ str {bpsFields = mappedFields}

    -- Helper function to annotate pattern matched fields
    bindedFieldsAnnotator :: BindedFields -> State AnnotatedUUID BindedFields
    bindedFieldsAnnotator bindedFs@BindedFields {bindedFields} = do
      mappedFields <- mapM bindFldAnnotator bindedFields
      return bindedFs {bindedFields = mappedFields}

    -- Helper function to annotate a single matched field
    bindFldAnnotator :: BindedField -> State AnnotatedUUID BindedField
    -- A BindedField should have a UUID only if it has no inner binding.
    -- This is because if an inner binding is present, this identifier is not added to the scope
    bindFldAnnotator bindedField@BindedField {bindFieldInnerBind = Nothing} = do
      curr <- get
      put $ curr + 1
      return bindedField {bindedFieldUUID = Just curr}
    bindFldAnnotator other' = return other'

-- |
-- Given any expression and the accumulated scopes, tries to infer the type of the expression
inferExprType :: Expr -> [Scope] -> Type
-- Binary expressions
inferExprType (BinaryOpExprExpr (Or _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (And _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Eq _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Neq _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Lt _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Gt _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Leq _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (Geq _ _)) _ = booleanType
inferExprType (BinaryOpExprExpr (BitwiseOr _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (BitwiseXor _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (BitwiseAnd _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (ShiftLeft _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (ShiftRight _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (Add _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (Sub _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (Mult _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (Div _ _)) _ = numericType
inferExprType (BinaryOpExprExpr (Mod _ _)) _ = numericType
--  An assignment has unit type
inferExprType (AssignmentExpr _) _ = unitType
-- Unary expressions
inferExprType (UnaryOpExpr (Negation _)) _ = booleanType
-- References and dereferences
inferExprType (UnaryOpExpr (MutableReference ref)) scopes = TypeMutableRef $ inferExprType ref scopes
inferExprType (UnaryOpExpr (ImmutableReference ref)) scopes = TypeImmutableRef $ inferExprType ref scopes
inferExprType (UnaryOpExpr (Dereference expr)) scopes =
  case inferExprType expr scopes of
    (TypeMutableRef refType) -> refType
    (TypeImmutableRef refType) -> refType
    exprType -> error $ "Dereferencing non-ref type: " ++ show expr ++ " with type: " ++ show exprType
-- move and copy espressions
inferExprType (UnaryOpExpr (MoveExpr expr)) scopes = inferExprType (NameAccessChainExpr $ LocalNameAccessChain expr) scopes
inferExprType (UnaryOpExpr (CopyExpr expr)) scopes = inferExprType (NameAccessChainExpr $ LocalNameAccessChain expr) scopes
-- Dot or index chain
inferExprType expr@(DotOrIndexChainExpr (DotAccess {dotAccessLeft, dotAccessRight})) scopes =
  case inferExprType dotAccessLeft scopes of
    -- Usually, the left part of the dot access is a named struct
    TypeConstructor typeCons typeArgs -> resolveDotAccess typeCons typeArgs
    -- But it can also be a reference (to a named struct)
    -- NOTE: Here it is not possible to know if that reference will be extended or if it is the syntactic sugar for dereferencing
    -- Meaning the difference between `r.a[.b]` and `& r.a[.b]`
    -- So the resulting type here should not consider the reference
    -- Or, to put in another way, default to the desugaring
    TypeImmutableRef (TypeConstructor typeCons typeArgs) -> resolveDotAccess typeCons typeArgs
    TypeMutableRef (TypeConstructor typeCons typeArgs) -> resolveDotAccess typeCons typeArgs
    exprType -> error $ "Dot access to non-struct type: " ++ show expr ++ " with type: " ++ show exprType
  where
    resolveDotAccess :: NameAccessChain -> [Type] -> Type
    resolveDotAccess nac typeArgs =
      case getNacFromScopes nac scopes of
        VariableAnnotations _ t@(IntermediateTypeNamedStructDeclaration typeParams namedFields) ->
          if length typeParams /= length typeArgs
            then error $ "Found a type constructor with wrong number of type arguments: " ++ show t
            else case List.find (\NamedField {fieldIdentifier} -> fieldIdentifier == dotAccessRight) namedFields of
              Nothing -> error $ "Found a dotAccessRight not present in the corresponding named struct declaration: " ++ show dotAccessLeft ++ ", " ++ show t
              Just NamedField {fieldType} -> resolveParametricType fieldType (zip typeParams typeArgs)
        _ -> error $ "Found dotAccessLeft that is not a named struct: " ++ show dotAccessLeft

-- Literal values
inferExprType (ValueLiteral (Address _)) _ = addressType
inferExprType (ValueLiteral (Boolean _)) _ = booleanType
inferExprType (ValueLiteral (Numerical _)) _ = numericType
-- Comma expression
-- If the tuple has just one expression, it is treated as a single value since parantesis act just for operation priority
inferExprType (CommaExpr [singleExpr]) scopes = inferExprType singleExpr scopes
inferExprType (CommaExpr exprs) scopes = TypeTuple $ map (`inferExprType` scopes) exprs
-- Typed expression
inferExprType (TypedExprTerm (TypedExpr {typedExprType})) _ = typedExprType
-- Casting
inferExprType (CastingTerm (Casting {castingType})) _ = castingType
-- Named struct expression
inferExprType (NamedStructExprExpr expr@(NamedStructExpr {nseNameAccessChain, nseTypeArgs})) scopes =
  case getNacFromScopes nseNameAccessChain scopes of
    VariableAnnotations _ (IntermediateTypeNamedStructDeclaration typeParams _) ->
      -- TODO: For now, inference of type arguments is not performed
      if length typeParams /= length nseTypeArgs
        then error $ "Found a named struct expression with wrong number of type arguments: " ++ show expr
        else TypeConstructor nseNameAccessChain nseTypeArgs
    _ -> error $ "Found a named struct expression that does not correspond to a struct declaration: " ++ show expr
-- Positional struct expression or function call
-- TODO: type parameters for now are ignored (for structs)
-- FIXME: follow what done for named structs
-- FIXME: There seems to be more logic here, to resolve parametric types. Probably add to the named struct expression above
inferExprType (PositionalStructExprOrFunctionCallExpr expr@(PositionalStructExprOrFunctionCall {pseofcNameAccessChain, pseofcTypeArgs})) scopes =
  case getNacFromScopes pseofcNameAccessChain scopes of
    -- The type of a function call is the return type.
    -- It is assumed that the correct number of arguments is passed
    VariableAnnotations _ (TypeArrow typeParams ts) ->
      -- TODO: For now, inference of type arguments is not performed
      if length typeParams /= length pseofcTypeArgs
        then error $ "Inferring type arguments is currently not supported, or mismatching number of type arguments: " ++ show expr
        -- Resolve the return type of the function
        else resolveParametricType (last ts) (zip typeParams pseofcTypeArgs)
    -- Otherwise, for a struct, it is the name of the struct
    VariableAnnotations _ structType -> structType
-- Function bang call has unit type
inferExprType (FunctionBangCallExpr _) _ = unitType
-- Name access chain
inferExprType (NameAccessChainExpr nac) scopes =
  let VariableAnnotations _ identType = getNacFromScopes nac scopes
   in identType
-- Sequence
inferExprType (SequenceExpr (Sequence {sequenceEndExpr})) scopes =
  case sequenceEndExpr of
    Nothing -> unitType
    Just expr -> inferExprType expr scopes
-- If then else
inferExprType (IfThenElseTerm (IfThenElse {ifThenElseIfBranch})) scopes = inferExprType ifThenElseIfBranch scopes
-- While
inferExprType (WhileTerm _) _ = unitType
-- Loop
inferExprType (Loop _) _ = TypeUnknown
-- Return
inferExprType (Return expr) scopes =
  case expr of
    Nothing -> unitType
    Just expr' -> inferExprType expr' scopes
-- Abort
inferExprType (Abort expr) scopes = inferExprType expr scopes
-- Break
inferExprType Break _ = TypeUnknown
-- Continue
inferExprType Continue _ = TypeUnknown
-- Intermediate AST expressions
inferExprType (IntermediateExprExpr (IntermediateReferenceLocalState _ _ exprType)) _ = exprType
inferExprType (IntermediateExprExpr (IntermediateGetDereferenceLocalState _ exprType)) _ = exprType
inferExprType (IntermediateExprExpr (IntermediatePutDereferenceLocalState {})) _ = IntermediateTypeScopes
inferExprType (IntermediateExprExpr (IntermediateGetLocalState _ exprType)) _ = exprType
inferExprType (IntermediateExprExpr (IntermediatePutLocalState {})) _ = IntermediateTypeScopes
inferExprType (IntermediateExprExpr (IntermediatePostLocalState _ _)) _ = IntermediateTypeScopes
inferExprType (IntermediateExprExpr (IntermediatePushScope _)) _ = IntermediateTypeScopes
inferExprType (IntermediateExprExpr (IntermediatePopScope _)) _ = IntermediateTypeScopes
inferExprType (IntermediateExprExpr (IntermediateBorrowGlobalMut _ t _)) _ = TypeMutableRef t
inferExprType (IntermediateExprExpr (IntermediateBorrowGlobal _ t _)) _ = TypeImmutableRef t
inferExprType (IntermediateExprExpr (IntermediateExists {})) _ = booleanType
inferExprType (IntermediateExprExpr (IntermediateMoveTo {})) _ = TypeTuple [TypeTuple [], IntermediateTypeScopes]
inferExprType (IntermediateExprExpr (IntermediateMoveFrom _ t _)) _ = TypeTuple [t, IntermediateTypeScopes]
inferExprType (IntermediateExprExpr (IntermediateTypeWitnessExprExpr _)) _ = IntermediateTypeWitnessType
inferExprType (IntermediateExprExpr expr@(IntermediateScopeBinding _)) _ = error $ "An IntermediateScopeBinding should never be present in the AST: " ++ show expr
inferExprType (IntermediateExprExpr (IntermediateReferenceExtension _ _ t)) _ = t
inferExprType (IntermediateExprExpr (IntermediateAsData _)) _ = IntermediateTypeData

-- |
-- Given a type that might be parametric, along with the type parameters of the record (or function) and their respective type arguments,
-- resolves the type by substituting the type arguments where a type param appears in the record type
resolveParametricType :: Type -> [(Identifier, Type)] -> Type
-- `T` can either be a defined type itself or be a type parameter
-- ```struct A<T>{label1: T}```
resolveParametricType t@(TypeConstructor (LocalNameAccessChain tCons) []) parentTArgs = fromMaybe t $ findMap (\(ident, typ) -> if tCons == ident then Just typ else Nothing) parentTArgs
  where
    -- \|
    -- Combination of List.find and List.map
    findMap :: (a -> Maybe b) -> [a] -> Maybe b
    findMap _ [] = Nothing
    findMap mapper (x : xs) = case mapper x of
      mapped@(Just _) -> mapped
      Nothing -> findMap mapper xs
-- If the type is not a local name, surely is not a type param
resolveParametricType t@(TypeConstructor _ []) _ = t
-- Otherwise it is a type where the constructor surely is not a type param, and the arguments might be
-- ```struct A<T>{label1: B<T>}```
resolveParametricType (TypeConstructor tCons tArgs) parentTArgs = TypeConstructor tCons (map (`resolveParametricType` parentTArgs) tArgs)
-- In a record label, the type must be one of the above cases. Tuples, references and other combinations not above are not allowed
-- But in case of function return, for example, it is possible to have tuples
resolveParametricType (TypeImmutableRef t) parentTArgs = TypeImmutableRef $ resolveParametricType t parentTArgs
resolveParametricType (TypeMutableRef t) parentTArgs = TypeMutableRef $ resolveParametricType t parentTArgs
resolveParametricType (TypeTuple t) parentTArgs = TypeTuple $ map (`resolveParametricType` parentTArgs) t
resolveParametricType TypeUnknown _ = TypeUnknown
resolveParametricType IntermediateTypeScopes _ = IntermediateTypeScopes
resolveParametricType t@(TypeArrow _ _) _ = error $ "Unexpected (possibly parametric) type: " ++ show t
resolveParametricType t@(IntermediateTypeNamedStructDeclaration _ _) _ = error $ "Unexpected (possibly parametric) type: " ++ show t
resolveParametricType IntermediateTypeWitnessType _ = IntermediateTypeWitnessType
resolveParametricType IntermediateTypeData _ = IntermediateTypeData

-- |
-- Tries to execute an IO operation, intercepting any error if thrown and providing additional informations
tryIO :: String -> IO a -> IO a
tryIO stepName step = do
  res <- try step
  case res of
    Left err -> error $ "Failed at " ++ stepName ++ ": " ++ displayException (err :: ErrorCall)
    Right val -> pure val

-- |
-- Checks if a type occurrence is parametric or not,
-- based on the identifiers of the type parameters defined on that function signature
isTypeParametric :: Type -> Set.Set Identifier -> Bool
isTypeParametric (TypeConstructor (LocalNameAccessChain tCons) tArgs) tParams = Set.member tCons tParams || any (`isTypeParametric` tParams) tArgs
isTypeParametric (TypeConstructor _ tArgs) tParams = any (`isTypeParametric` tParams) tArgs
isTypeParametric (TypeImmutableRef t) tParams = isTypeParametric t tParams
isTypeParametric (TypeMutableRef t) tParams = isTypeParametric t tParams
isTypeParametric (TypeTuple ts) tParams = any (`isTypeParametric` tParams) ts
isTypeParametric TypeUnknown _ = False
isTypeParametric IntermediateTypeScopes _ = False
-- NOTE: An arrow type might be parametric on some outer type names, but in this case should never be encountered in any case
isTypeParametric (TypeArrow _ _) _ = False
-- A struct declaration type should never be encountered
isTypeParametric t@(IntermediateTypeNamedStructDeclaration _ _) _ = error $ "Unexpected type: " ++ show t
isTypeParametric IntermediateTypeWitnessType _ = False
isTypeParametric IntermediateTypeData _ = False

-- |
-- Used mainly to perform traversal over multiple ASTs
--
-- What it does is, given a mapper function and a list of elements to loop on,
-- iterates over each element, providing all the others as a second argument for the mapper function.
-- During the iteration, forwards a custom state and collects all the intermediate results,
-- returning the final state and a list with all the mapped results
iterateWithOthers :: (a -> [a] -> State st b) -> [a] -> State st [b]
iterateWithOthers _ [] = return []
iterateWithOthers func (x : xs) = iterateWithOthers' func [] x xs

-- |
-- Helper function for `iterateWithOthers`
-- Accepts the mapper function, the previous (already mapped on) elements, the current element to map and the next elements yet to map
iterateWithOthers' :: (a -> [a] -> State st b) -> [a] -> a -> [a] -> State st [b]
iterateWithOthers' func prev curr [] = do
  res <- func curr prev
  return [res]
iterateWithOthers' func prev curr next@(n : ns) = do
  res <- func curr (prev ++ next)
  resN <- iterateWithOthers' func (prev ++ [curr]) n ns
  return (res : resN)

-- Helper function to perform the union of a list of sets
unionSet :: (Ord a) => [Set.Set a] -> Set.Set a
unionSet = foldr Set.union Set.empty