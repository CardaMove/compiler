module Move.Translations.Utils where

import Control.Monad.State
  ( MonadState (get, put),
    State,
  )
import Data.Generics.Uniplate.Data (transformBiM)
import Data.Map qualified as Map
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
type Scope = Map.Map Identifier VariableAnnotations

-- |
-- Checks if the identifier is present in any of the input scopes
isIdentifierInScope :: Identifier -> [Scope] -> Bool
isIdentifierInScope _ [] = False
isIdentifierInScope ident (x : xs) = Map.member ident x || isIdentifierInScope ident xs

-- |
-- Returns the UUID and inferred type of an identifier present in the scope.
--
-- Throws an error if the identifier is not present in the scope
getIdentifierFromScopes :: Identifier -> [Scope] -> VariableAnnotations
getIdentifierFromScopes ident [] = error ("Cannot get identifier type. Not in scope: " ++ show ident)
getIdentifierFromScopes ident (x : xs) = case Map.lookup ident x of
  -- The identifier is not in this scope, proceed recursively
  Nothing -> getIdentifierFromScopes ident xs
  -- The identifier is in this scope, return its UUID and type (if available)
  Just res -> res

-- |
-- Given a Use, returns all the alias identifiers.
--
-- Also handles the use members and the Self member
getUseIdentifiers :: Use -> [Identifier]
getUseIdentifiers use = case use of
  Use {useIdentifier, useAlias, useMembers = []} -> [getIdentifier useAlias useIdentifier]
  Use {useIdentifier, useMembers = [UseMember {useMemberIdentifier = Identifier "Self", useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useIdentifier]
  Use {useMembers = [UseMember {useMemberIdentifier, useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useMemberIdentifier]
  use'@Use {useMembers = x : xs} -> getUseIdentifiers use' {useMembers = [x]} ++ getUseIdentifiers use' {useMembers = xs}
  where
    getIdentifier :: Maybe Identifier -> Identifier -> Identifier
    getIdentifier Nothing def = def
    getIdentifier (Just ident) _ = ident

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
annotateBindingsWithUUID :: Root -> State Int Root
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
    handleTopLevels :: [TopLevel] -> State Int [TopLevel]
    handleTopLevels topLevels = do mapM topLevelAnnotator topLevels

    -- Annotates any top level
    topLevelAnnotator :: TopLevel -> State Int TopLevel
    topLevelAnnotator (TopLevelFunction topLFunction@Function {functionParameters, functionBody}) = do
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
              functionBody = annotatedBody
            }
    -- Every other top level node is ignored
    topLevelAnnotator otherTL = return otherTL

    -- Annotates all the bindings in the function body
    functionBodyAnnotator :: Bind -> State Int Bind
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
    bindedFieldsAnnotator :: BindedFields -> State Int BindedFields
    bindedFieldsAnnotator bindedFs@BindedFields {bindedFields} = do
      mappedFields <- mapM bindFldAnnotator bindedFields
      return bindedFs {bindedFields = mappedFields}

    -- Helper function to annotate a single matched field
    bindFldAnnotator :: BindedField -> State Int BindedField
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
-- TODO: Consider about extending other references via &my_ref.b.c
inferExprType (UnaryOpExpr (MutableReference ref)) scopes = TypeMutableRef $ inferExprType ref scopes
inferExprType (UnaryOpExpr (ImmutableReference ref)) scopes = TypeImmutableRef $ inferExprType ref scopes
inferExprType (UnaryOpExpr (Dereference expr)) scopes =
  case inferExprType expr scopes of
    (TypeMutableRef refType) -> refType
    (TypeImmutableRef refType) -> refType
    _ -> error "Dereferencing non-ref type"
inferExprType (UnaryOpExpr (MoveExpr expr)) scopes = inferExprType (NameAccessChainExpr $ LocalNameAccessChain expr) scopes
inferExprType (UnaryOpExpr (CopyExpr expr)) scopes = inferExprType (NameAccessChainExpr $ LocalNameAccessChain expr) scopes
-- Dot or index chain TODO:
inferExprType (DotOrIndexChainExpr (DotAccess {dotAccessLeft, dotAccessRight})) scopes =
  case inferExprType dotAccessLeft scopes of
    TypeConstructor typeCons typeArgs -> TypeUnknown
    TypeImmutableRef refType -> TypeUnknown -- Should be a TypeImmutableRef itself
    TypeMutableRef refType -> TypeUnknown -- Should be a TypeMutableRef itself
    _ -> error "Dot access to non-struct type"
-- Literal values
inferExprType (ValueLiteral (Address _)) _ = addressType
inferExprType (ValueLiteral (Boolean _)) _ = booleanType
inferExprType (ValueLiteral (Numerical _)) _ = numericType
-- Comma expression
inferExprType (CommaExpr exprs) scopes = TypeTuple $ map (`inferExprType` scopes) exprs
-- Typed expression
inferExprType (TypedExprTerm (TypedExpr {typedExprType})) _ = typedExprType
-- Casting
inferExprType (CastingTerm (Casting {castingType})) _ = castingType
-- Named struct expression TODO:
inferExprType (NamedStructExprExpr (NamedStructExpr {nseNameAccessChain})) scopes = TypeUnknown
-- Positional struct expression or function call TODO:
inferExprType (PositionalStructExprOrFunctionCallExpr (PositionalStructExprOrFunctionCall {pseofcNameAccessChain})) scopes = TypeUnknown
-- Function bang call has unit type
inferExprType (FunctionBangCallExpr _) _ = unitType
-- Name access chain
inferExprType (NameAccessChainExpr (LocalNameAccessChain ident)) scopes =
  let VariableAnnotations _ maybeType = getIdentifierFromScopes ident scopes
   in maybeType
-- TODO: resolve alias
inferExprType (NameAccessChainExpr (AliasedNameAccessChain address ident)) scopes = TypeUnknown
-- TODO: resolve alias
inferExprType (NameAccessChainExpr (UnaliasedNameAccessChain address ident1 ident2)) scopes = TypeUnknown
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
