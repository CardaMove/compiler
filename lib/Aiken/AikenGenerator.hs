{-# LANGUAGE QuasiQuotes #-}

module Aiken.AikenGenerator (generateRoot, generateTopLevel) where

import Control.Exception (ErrorCall, Exception (displayException), evaluate, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text, empty, intercalate, null, pack)
import Move.AST
import Move.Translations.Utils (unitType)
import NeatInterpolation (trimming)

-- |
-- Tries to execute an IO operation, intercepting any error if thrown and providing additional informations
runStep :: String -> IO a -> IO a
runStep stepName step = do
  res <- try step
  case res of
    Left err -> error $ "Aiken generator failed at step " ++ stepName ++ ": " ++ displayException (err :: ErrorCall)
    Right val -> pure val

-- |
-- Given either a Script or a Module, generates its corresponding Aiken code
--
-- NOTE: the default indentation size for Aiken is two spaces
generateRoot :: FilePath -> Root -> IO Text
generateRoot fileName (RModule Module {moduleTopLevels}) =
  runStep fileName $ evaluate $ join "\n" $ filter (not . Data.Text.null) $ map generateTopLevel moduleTopLevels
generateRoot fileName (RScript Script {scriptTopLevels}) =
  runStep fileName $ evaluate $ join "\n" $ filter (not . Data.Text.null) $ map generateTopLevel scriptTopLevels

-- |
-- Given any top level, generates its corresponding Aiken code
-- TODO: Add support for Use
generateTopLevel :: TopLevel -> Text
generateTopLevel (TopLevelUse _) = error "Top level Use are not currently supported"
-- Friends have no translations
generateTopLevel (TopLevelFriend _) = empty
generateTopLevel (TopLevelNamedStruct NamedStruct {namedStructIdentifier, namedStructTypeParameters, namedStructFields}) =
  [trimming|
    pub type $name$tParams {
      $fields
    }
  |]
  where
    name = packIdent namedStructIdentifier
    tParams = packTypeParams namedStructTypeParameters
    fields = join "" $ map packNamedField namedStructFields

    -- Translates a field of a named struct into Aiken
    -- NOTE that each field ends with a final ',' even if it is the last
    packNamedField :: NamedField -> Text
    packNamedField NamedField {fieldIdentifier, fieldType} =
      [trimming|
        $fieldName: $packedType,
      |]
      where
        fieldName :: Text = packIdent fieldIdentifier
        packedType :: Text = generateType fieldType
generateTopLevel (TopLevelPositionalStruct PositionalStruct {positionalStructIdentifier, positionalStructTypeParameters, positionalStructFields}) =
  [trimming|
    pub type $name$tParams {
      $name($fields)
    }
  |]
  where
    name = packIdent positionalStructIdentifier
    tParams = packTypeParams positionalStructTypeParameters
    fields = intercalate (pack ", ") $ map packPositionalField positionalStructFields

    packPositionalField :: PositionalField -> Text
    packPositionalField (PositionalField t) = generateType t
generateTopLevel (TopLevelConstant Constant {constantIdentifier, constantType, constantExpression}) =
  [trimming|
    const $name: $t = $expr
  |]
  where
    name = packIdent constantIdentifier
    t = generateType constantType
    expr = generateExpression constantExpression
-- Function declaration
-- Note that type parameters should not be specified in Aiken
generateTopLevel (TopLevelFunction Function {functionHasNativeModifier = True}) = error "Native functions are not supported"
generateTopLevel (TopLevelFunction Function {functionHasNativeModifier = False, functionVisibilityModifier, functionName, functionParameters, functionReturnType, functionBody}) =
  [trimming|
    ${visibility}fn $name($fParams) -> $fReturn {
      $fBody
    }
  |]
  where
    visibility = case functionVisibilityModifier of
      Nothing -> empty
      _ -> pack "pub "

    name = packIdent functionName
    fParams = intercalate (pack ", ") $ map packFunctionParameter functionParameters

    -- Functions with no return type default to unit
    fReturn = case functionReturnType of
      Nothing -> generateType unitType
      Just t -> generateType t

    -- Functions with no body default to returning unit
    fBody = case functionBody of
      Nothing -> generateExpression $ CommaExpr []
      Just sqnc -> generateSequence sqnc

    packFunctionParameter :: Parameter -> Text
    packFunctionParameter Parameter {parameterIdentifier, parameterType} =
      [trimming|
        $paramName: $paramType
      |]
      where
        paramName = packIdent parameterIdentifier
        paramType = generateType parameterType

-- |
-- Joins multiple code lines as a single Text,
-- adding a custom separator and a newline
join :: String -> [Text] -> Text
join sep = intercalate (pack $ sep ++ "\n")

-- |
-- Maps an Identifier to Text
packIdent :: Identifier -> Text
packIdent (Identifier ident) = pack ident

-- |
-- Converts a Type into the corresponding Aiken code
--
-- Includes translating Move stdlib types into Aiken types,
-- such as `u64`, `u256` into `Int`
-- and `bool` into `Bool`
-- TODO: Add support for non local names, + Address and Signer on libraries
generateType :: Type -> Text
generateType (TypeConstructor (LocalNameAccessChain (Identifier "bool")) []) = pack "Bool"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u8")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u16")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u32")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u64")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u128")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u256")) []) = pack "Int"
-- Address and Signer are mapped to custom Aiken types
generateType (TypeConstructor (LocalNameAccessChain (Identifier "address")) []) = pack "Address"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "signer")) []) = pack "Signer"
generateType (TypeConstructor (LocalNameAccessChain ident) tArgs) =
  [trimming|
    $name$packedTArgs
  |]
  where
    name = packIdent ident
    packedTArgs :: Text = packTypeArgs tArgs
    -- Generates Aiken code corresponding to the given type arguments
    -- TODO: Type arguments coming from type parameters should be lowercase.
    -- Add this logic to the transpiler
    packTypeArgs :: [Type] -> Text
    packTypeArgs [] = empty
    packTypeArgs tArgs' =
      [trimming|
        <$ts>
      |]
      where
        ts :: Text = intercalate (pack ", ") $ map generateType tArgs'
-- Any reference type is mapped to a custom Aiken type
-- TODO: Add support in Aiken lib
generateType (TypeImmutableRef rType) =
  [trimming|
    Ref<$rType'>
  |]
  where
    rType' = generateType rType
generateType (TypeMutableRef rType) =
  [trimming|
    Ref<$rType'>
  |]
  where
    rType' = generateType rType
-- The unit type () is translated to Void, like the unit expression
generateType (TypeTuple []) = pack "Void"
generateType (TypeTuple ts) =
  [trimming|
    ($ts')
  |]
  where
    ts' = intercalate (pack ", ") $ map generateType ts
-- Type witness types are mapped to a custom Aiken type
-- TODO: Add support in Aiken lib
generateType IntermediateTypeWitnessType = pack "TWitness"
-- Scopes are mapped to a custom Aiken type
generateType IntermediateTypeScopes = pack "List<Scope>"
-- It is possible that some types can not be inferred, so use a custom UNKNOWN_TYPE
-- TODO: It might be an alias for Data, but in any case it would not compile
generateType TypeUnknown = pack "UNKNOWN_TYPE"
generateType t = error $ "Unexpected type: " ++ show t

-- |
-- Given some type parameters, generates the corresponding Aiken code
-- consisting in `<...>`.
--
-- TODO: Note that it does not enforce the identifiers to be lowercase,
-- Since it has to be done by the transpiler
packTypeParams :: [TypeParameter] -> Text
packTypeParams [] = empty
packTypeParams tParams =
  [trimming|
    <$ts>
  |]
  where
    ts :: Text = intercalate (pack ", ") $ map (packIdent . typeIdentifier) tParams

-- |
-- Given any Move expression, generates its corresponding Aiken expression
-- TODO:
generateExpression :: Expr -> Text
-- Binary operations
generateExpression (BinaryOpExprExpr (Or left right)) = binaryOpHelper left "||" right
generateExpression (BinaryOpExprExpr (And left right)) = binaryOpHelper left "&&" right
generateExpression (BinaryOpExprExpr (Eq left right)) = binaryOpHelper left "==" right
generateExpression (BinaryOpExprExpr (Neq left right)) = binaryOpHelper left "!=" right
generateExpression (BinaryOpExprExpr (Lt left right)) = binaryOpHelper left "<" right
generateExpression (BinaryOpExprExpr (Gt left right)) = binaryOpHelper left ">" right
generateExpression (BinaryOpExprExpr (Leq left right)) = binaryOpHelper left "<=" right
generateExpression (BinaryOpExprExpr (Geq left right)) = binaryOpHelper left ">=" right
generateExpression (BinaryOpExprExpr (BitwiseOr _ _)) = error "Bitwise operators unsupported"
generateExpression (BinaryOpExprExpr (BitwiseXor _ _)) = error "Bitwise operators unsupported"
generateExpression (BinaryOpExprExpr (BitwiseAnd _ _)) = error "Bitwise operators unsupported"
generateExpression (BinaryOpExprExpr (ShiftLeft _ _)) = error "Bitshift operators unsupported"
generateExpression (BinaryOpExprExpr (ShiftRight _ _)) = error "Bitshift operators unsupported"
generateExpression (BinaryOpExprExpr (Add left right)) = binaryOpHelper left "+" right
generateExpression (BinaryOpExprExpr (Sub left right)) = binaryOpHelper left "-" right
generateExpression (BinaryOpExprExpr (Mult left right)) = binaryOpHelper left "*" right
generateExpression (BinaryOpExprExpr (Div left right)) = binaryOpHelper left "/" right
generateExpression (BinaryOpExprExpr (Mod left right)) = binaryOpHelper left "%" right
-- Assignments should not be present
generateExpression expr@(AssignmentExpr _) = error $ "Unexpected assignment expression: " ++ show expr
-- Unary operations
generateExpression (UnaryOpExpr (Negation inner)) =
  [trimming|
    !$inner'
  |]
  where
    inner' = generateExpression inner
-- References should not be present
generateExpression expr@(UnaryOpExpr (MutableReference _)) = error $ "Unexpected reference expression: " ++ show expr
generateExpression expr@(UnaryOpExpr (ImmutableReference _)) = error $ "Unexpected reference expression: " ++ show expr
generateExpression expr@(UnaryOpExpr (Dereference _)) = error $ "Unexpected dereference expression: " ++ show expr
generateExpression (UnaryOpExpr (MoveExpr ident)) = packIdent ident
generateExpression (UnaryOpExpr (CopyExpr ident)) = packIdent ident
generateExpression (DotOrIndexChainExpr DotAccess {dotAccessLeft, dotAccessRight}) =
  [trimming|
    $left.$name
  |]
  where
    left = generateExpression dotAccessLeft
    name = packIdent dotAccessRight
-- TODO: Address literals
generateExpression (ValueLiteral (Address _)) = error "Address literals currently not supported"
-- Booleans
generateExpression (ValueLiteral (Boolean value)) = if value then pack "True" else pack "False"
generateExpression (ValueLiteral (Numerical (LiteralIntDec value))) = pack $ show value
generateExpression (ValueLiteral (Numerical (LiteralIntHex value))) = pack value
-- The unit expression is translated as Void, like the unit type
generateExpression (CommaExpr []) = pack "Void"
generateExpression (CommaExpr exprs) =
  [trimming|
    ($exprs')
  |]
  where
    exprs' = intercalate (pack ", ") $ map generateExpression exprs
generateExpression (TypedExprTerm TypedExpr {typedExpr}) = generateExpression typedExpr
-- Casting is translated as a non-exaustive pattern matchich via `expect`
generateExpression (CastingTerm Casting {castingExpr, castingType}) =
  [trimming|
    {
      expect val:$t = $expr
      val
    }
  |]
  where
    t = generateType castingType
    expr = generateExpression castingExpr
-- Named structs
-- Note that struct expressions in Aiken do not specify type arguments
generateExpression (NamedStructExprExpr NamedStructExpr {nseNameAccessChain, nseFields}) =
  [trimming|
    $name { $fields }
  |]
  where
    name = generateNameAccessChain nseNameAccessChain
    fields = intercalate (pack ", ") $ map packNamedStructField nseFields

    packNamedStructField :: NamedStructExprField -> Text
    packNamedStructField NamedStructExprField {nsefIdentifier, nsefExpr} =
      [trimming|
        $fieldName: $fieldExpr
      |]
      where
        fieldName = packIdent nsefIdentifier
        -- If the named field has no associated expression, it defaults to the field name itself
        fieldExpr = generateExpression $ fromMaybe (NameAccessChainExpr $ LocalNameAccessChain nsefIdentifier) nsefExpr
-- Positional structs or function calls
-- Note that neither struct expressions nor function calls in Aiken not specify type arguments
generateExpression (PositionalStructExprOrFunctionCallExpr PositionalStructExprOrFunctionCall {pseofcNameAccessChain, pseofcFields}) =
  [trimming|
    $name($fields)
  |]
  where
    name = generateNameAccessChain pseofcNameAccessChain
    fields = intercalate (pack ", ") $ map generateExpression pseofcFields
generateExpression (FunctionBangCallExpr FunctionBangCall {fbcNameAccessChain, fbcFields}) =
  [trimming|
    $name!($fields)
  |]
  where
    name = generateNameAccessChain fbcNameAccessChain
    fields = intercalate (pack ", ") $ map generateExpression fbcFields
generateExpression (NameAccessChainExpr nac) = generateNameAccessChain nac
-- Sequence expression
generateExpression (SequenceExpr sqn) =
  [trimming|
    {
      $sqn'
    }
  |]
  where
    sqn' = generateSequence sqn
-- If then else
generateExpression expr@(IfThenElseTerm IfThenElse {ifThenElseElseBranch = Nothing}) = error $ "Unexpected if-then-else with no else branch: " ++ show expr
generateExpression (IfThenElseTerm IfThenElse {ifThenElseCondition, ifThenElseIfBranch, ifThenElseElseBranch = Just ifThenElseElseBranch'}) =
  [trimming|
    if $cond $ifBranch else $elseBranch
  |]
  where
    cond = generateExpression ifThenElseCondition
    -- Both branches must be encosed between braces, so check if they are a SequenceExpr and add them manually if not
    ifBranch = case ifThenElseIfBranch of
      SequenceExpr _ -> generateExpression ifThenElseIfBranch
      _ ->
        [trimming|
          {
            $branch
          }
        |]
        where
          branch = generateExpression ifThenElseIfBranch
    elseBranch = case ifThenElseElseBranch' of
      SequenceExpr _ -> generateExpression ifThenElseElseBranch'
      _ ->
        [trimming|
          {
            $branch
          }
        |]
        where
          branch = generateExpression ifThenElseElseBranch'
-- Whiles should not be present
generateExpression expr@(WhileTerm _) = error $ "Unexpected while: " ++ show expr
-- Loops should not be present
generateExpression expr@(Loop _) = error $ "Unexpected loop: " ++ show expr
-- A return is just the expression itself, defaulting to unit if not present
generateExpression (Return expr) = generateExpression $ fromMaybe (CommaExpr []) expr
-- Since failing in Aiken restricts the message to a String, the aborting expression is simply stringified
generateExpression (Abort expr) =
  [trimming|
    fail @"Failing with custom expression $expr'"
  |]
  where
    expr' = pack $ show expr
generateExpression Break = error "Unexpected break"
generateExpression Continue = error "Unexpected continue"
-- Intermediate expressions TODO:
-- PUSH and POP operations on the scope
-- TODO: Add support in Aiken lib
generateExpression (IntermediateExprExpr (IntermediatePushScope ident)) =
  [trimming|
    push_scope($ident')
  |]
  where
    ident' = packIdent ident
generateExpression (IntermediateExprExpr (IntermediatePopScope ident)) =
  [trimming|
    pop_scope($ident')
  |]
  where
    ident' = packIdent ident

-- |
-- Given a Move Sequence (not SequenceExpr), generates the corresponding Aiken code,
-- without braces '{' '}'. Braces should be handled both by `generateExpression` and top level function
generateSequence :: Sequence -> Text
generateSequence sqn@Sequence {sequenceUses, sequenceItems, sequenceEndExpr} =
  if not $ Prelude.null sequenceUses
    then error $ "Unexpected sequence with Uses:" ++ show sqn
    else join "" $ map generateSqnItem sequenceItems ++ [generateExpression $ fromMaybe (CommaExpr []) sequenceEndExpr]
  where
    generateSqnItem :: SequenceItem -> Text
    generateSqnItem (SequenceItemExpr expr) = generateExpression expr
    generateSqnItem item@(SequenceItemBindExpr Bindings {bindingsBindExpr = Nothing}) = error $ "Unexpected binding without expression: " ++ show item
    generateSqnItem (SequenceItemBindExpr Bindings {bindings, bindingsBindType, bindingsBindExpr = Just bindingsBindExpr'}) =
      [trimming|
        let $bind'$bindType = $bindExpr
      |]
      where
        bind' = case bindings of
          BindedSingle bind'' -> generateBind bind''
          BindedTuple binds ->
            [trimming|
              ($binds')
            |]
            where
              binds' = intercalate (pack ", ") $ map generateBind binds
        bindType = case bindingsBindType of
          Nothing -> empty
          Just bindingsBindType' ->
            [trimming|
              : $t
            |]
            where
              t = generateType bindingsBindType'
        bindExpr = generateExpression bindingsBindExpr'

        -- Given a single bind, generates the corresponding Aiken code
        generateBind :: Bind -> Text
        generateBind (BindIdentifier ident _) = packIdent ident
        generateBind (BindNamedStruct BindedNamedStruct {bnsNameAccessChain, bnsFields}) =
          [trimming|
            $name { $fields }
          |]
          where
            name = generateNameAccessChain bnsNameAccessChain
            fields = packBindedFields bnsFields
        generateBind (BindPositionalStruct BindedPositionalStruct {bpsNameAccessChain, bpsFields}) =
          [trimming|
            $name ($fields)
          |]
          where
            name = generateNameAccessChain bpsNameAccessChain
            fields = packBindedFields bpsFields

        packBindedFields :: BindedFields -> Text
        packBindedFields bf@BindedFields {hasPartialPattern = True} = error $ "Binded fields with partial patterns are currently not supported: " ++ show bf
        packBindedFields BindedFields {hasPartialPattern = False, bindedFields} = intercalate (pack ", ") $ map packBindedField bindedFields

        packBindedField :: BindedField -> Text
        packBindedField BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing} = packIdent bindFieldIdentifier
        packBindedField BindedField {bindFieldIdentifier, bindFieldInnerBind = Just bindFieldInnerBind'} =
          [trimming|
            $name: $inner
          |]
          where
            name = packIdent bindFieldIdentifier
            inner = generateBind bindFieldInnerBind'

-- |
-- Given an access chain, generates the corresponding Aiken code
-- TODO: add support for non local access chain
generateNameAccessChain :: NameAccessChain -> Text
generateNameAccessChain (LocalNameAccessChain ident) = packIdent ident
generateNameAccessChain nac = error $ "Non local name access chains are currently not supported" ++ show nac

-- |
-- Helper for generating Aiken for binary operations
--
-- It accepts the left and right operands and the String representation of the operator,
-- returning `left op right`
binaryOpHelper :: Expr -> String -> Expr -> Text
binaryOpHelper left op right =
  [trimming|
    $left' $op' $right'
  |]
  where
    left' = generateExpression left
    op' = pack op
    right' = generateExpression right