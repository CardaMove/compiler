{-# LANGUAGE QuasiQuotes #-}

module Aiken.AikenGenerator (generateRoot, generateTopLevel) where

import Control.Exception (ErrorCall, Exception (displayException), evaluate, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text, empty, intercalate, null, pack)
import Move.AST
import Move.Translations.PostProcessing (gsUtilsIdent, refUtilsIdent, scopeUtilsIdent)
import Move.Translations.Utils (cpsIdentifier, unitType)
import NeatInterpolation (trimming)

--
-- TODO: NOTE: some cases might be rewritten by a final translation step of the AST
--
-- TODO: Signer module?

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
generateTopLevel :: TopLevel -> Text
-- NOTE: module addresses are translated in folder names.
-- TODO: this might be insufficient for named addresses since their value is set in the Toml file?
generateTopLevel (TopLevelUse Use {useAddress, useIdentifier, useAlias, useMembers}) =
  [trimming|
    use $addr/$ident$members$alias
  |]
  where
    -- Only in this case, the identifier should not be encosed by string quotes
    -- since just the name is needed
    -- So `packAddress` is not used
    addr = case useAddress of
      NamedAddress addrIdent -> packIdent addrIdent
      NumericalAddress (LiteralIntDec numAddr) -> pack $ show numAddr
      NumericalAddress (LiteralIntHex numAddr) -> pack numAddr
    ident = packIdent useIdentifier
    alias = case useAlias of
      Nothing -> empty
      Just alias' -> pack " as " <> packIdent alias'

    -- TODO: use members alias are currently not supported
    members = case useMembers of
      [] -> empty
      _ ->
        [trimming|
          .{$members'}
        |]
        where
          members' = intercalate (pack ", ") $ map (packIdent . useMemberIdentifier) useMembers
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
generateType :: Type -> Text
generateType (TypeConstructor (LocalNameAccessChain (Identifier "bool")) []) = pack "Bool"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u8")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u16")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u32")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u64")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u128")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u256")) []) = pack "Int"
-- Address and Signer are mapped to custom Aiken types
generateType (TypeConstructor (LocalNameAccessChain (Identifier "address")) []) = pack "Addr"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "signer")) []) = pack "Signer"
generateType (TypeConstructor nac tArgs) =
  [trimming|
    $name$packedTArgs
  |]
  where
    name = generateNameAccessChain nac
    packedTArgs :: Text = packTypeArgs tArgs
    -- Generates Aiken code corresponding to the given type arguments
    -- Note that type params are expected to be already in snake_case
    packTypeArgs :: [Type] -> Text
    packTypeArgs [] = empty
    packTypeArgs tArgs' =
      [trimming|
        <$ts>
      |]
      where
        ts :: Text = intercalate (pack ", ") $ map generateType tArgs'
-- Any reference type is mapped to a custom Aiken type
generateType (TypeImmutableRef rType) =
  [trimming|
    Reference<$rType'>
  |]
  where
    rType' = generateType rType
generateType (TypeMutableRef rType) =
  [trimming|
    Reference<$rType'>
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
generateType IntermediateTypeWitnessType = pack "TWitness"
-- Scopes are mapped to a custom Aiken type
generateType IntermediateTypeScopes = pack "CPS"
-- It is possible that some types can not be inferred, so use a custom UNKNOWN_TYPE
-- Note that this prevents compiling the Aiken code
generateType TypeUnknown = pack "UNKNOWN_TYPE"
generateType t@(TypeArrow _ _) = error $ "Unexpected TypeArrow: " ++ show t
generateType t@(IntermediateTypeNamedStructDeclaration _ _) = error $ "Unexpected IntermediateTypeNamedStructDeclaration: " ++ show t

-- |
-- Given some type parameters, generates the corresponding Aiken code
-- consisting in `<...>`.
--
-- Note that type params are expected to be already in snake_case
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
generateExpression (ValueLiteral (Address addr)) = packAddress addr
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
-- Note that neither struct expressions nor function calls in Aiken do specify type arguments
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
--
-- Intermediate expressions
--
generateExpression (IntermediateExprExpr (IntermediateReferenceLocalState ident fields _)) =
  [trimming|
    $lib.make_ref($ident', $fields', $cpsIdentifier')
  |]
  where
    lib = packIdent refUtilsIdent
    ident' = packIdent ident
    fields' = pack $ show fields
    cpsIdentifier' = packIdent cpsIdentifier
-- Any `*a` on the right side
-- Requires a manual casting after calling the Aiken lib
generateExpression (IntermediateExprExpr (IntermediateGetDereferenceLocalState expr t)) =
  [trimming|
    {
      expect val: $casted' = $lib.deref($expr', $cpsIdentifier')
      val
    }
  |]
  where
    casted' = generateType t
    lib = packIdent refUtilsIdent
    expr' = generateExpression expr
    cpsIdentifier' = packIdent cpsIdentifier
generateExpression (IntermediateExprExpr (IntermediatePutDereferenceLocalState ident expr fields)) =
  [trimming|
    $lib.put_deref($ident', $fields', $expr', $cpsIdentifier')
  |]
  where
    lib = packIdent refUtilsIdent
    ident' = packIdent ident
    fields' = pack $ show fields
    expr' = generateExpression expr
    cpsIdentifier' = packIdent cpsIdentifier
-- Any `a[.b.c]` where `a` is in the local state
-- Requires a manual casting after calling the Aiken lib
generateExpression (IntermediateExprExpr (IntermediateGetLocalState ident t)) =
  [trimming|
    {
      expect val: $casted' = $lib.get_scope($ident', $cpsIdentifier')
      val
    }
  |]
  where
    casted' = generateType t
    lib = packIdent scopeUtilsIdent
    ident' = packIdent ident
    cpsIdentifier' = packIdent cpsIdentifier
generateExpression (IntermediateExprExpr (IntermediatePutLocalState ident expr fields)) = 
  [trimming|
    $lib.put_scope($ident', $fields', $expr', $cpsIdentifier')
  |]
  where
    lib = packIdent scopeUtilsIdent
    ident' = packIdent ident
    fields' = pack $ show fields
    expr' = generateExpression expr
    cpsIdentifier' = packIdent cpsIdentifier
-- Any `a[.b.c] = ...`
generateExpression (IntermediateExprExpr (IntermediatePostLocalState ident expr)) =
  [trimming|
    $lib.post_scope($ident', $expr', $cpsIdentifier')
  |]
  where
    lib = packIdent scopeUtilsIdent
    ident' = packIdent ident
    expr' = generateExpression $ fromMaybe (CommaExpr []) expr
    cpsIdentifier' = packIdent cpsIdentifier
-- PUSH and POP operations on the scope
generateExpression (IntermediateExprExpr (IntermediatePushScope ident)) =
  [trimming|
    $lib.push_scope($ident')
  |]
  where
    lib = packIdent scopeUtilsIdent
    ident' = packIdent ident
generateExpression (IntermediateExprExpr (IntermediatePopScope ident)) =
  [trimming|
    $lib.pop_scope($ident')
  |]
  where
    lib = packIdent scopeUtilsIdent
    ident' = packIdent ident
-- `borrow_global_mut<T>(address)`
generateExpression (IntermediateExprExpr (IntermediateBorrowGlobalMut expr _t tWitness)) =
  [trimming|
    $lib.borrow_global($addr, $resourceType, $cpsIdentifier')
  |]
  where
    lib = packIdent gsUtilsIdent
    addr = generateExpression expr
    resourceType = generateTWitness tWitness
    cpsIdentifier' = packIdent cpsIdentifier
-- `borrow_global<T>(address)`
generateExpression (IntermediateExprExpr (IntermediateBorrowGlobal expr _t tWitness)) =
  [trimming|
    $lib.borrow_global($addr, $resourceType, $cpsIdentifier')
  |]
  where
    lib = packIdent gsUtilsIdent
    addr = generateExpression expr
    resourceType = generateTWitness tWitness
    cpsIdentifier' = packIdent cpsIdentifier
-- `exists<T>(address)`
generateExpression (IntermediateExprExpr (IntermediateExists expr _t tWitness)) =
  [trimming|
    $lib.exists($addr, $resourceType, $cpsIdentifier')
  |]
  where
    lib = packIdent gsUtilsIdent
    addr = generateExpression expr
    resourceType = generateTWitness tWitness
    cpsIdentifier' = packIdent cpsIdentifier
-- `move_to<T>(&signer, T)`
generateExpression (IntermediateExprExpr (IntermediateMoveTo signer expr _t tWitness)) =
  [trimming|
    $lib.move_to($signer', $resourceType, $expr', $cpsIdentifier')
  |]
  where
    lib = packIdent gsUtilsIdent
    signer' = generateExpression signer
    resourceType = generateTWitness tWitness
    expr' = generateExpression expr
    cpsIdentifier' = packIdent cpsIdentifier
-- `move_from<T>(address)`
-- Requires a manual casting after calling the Aiken lib
generateExpression (IntermediateExprExpr (IntermediateMoveFrom expr t tWitness)) =
  [trimming|
    {
      expect (val, $cpsIdentifier'): ($casted', $scopeT) = $lib.move_from($addr, $resourceType, $cpsIdentifier')
      (val, $cpsIdentifier')
    }
  |]
  where
    casted' = generateType t
    scopeT = generateType IntermediateTypeScopes
    lib = packIdent gsUtilsIdent
    addr = generateExpression expr
    resourceType = generateTWitness tWitness
    cpsIdentifier' = packIdent cpsIdentifier
-- Type witnesses expressions
generateExpression (IntermediateExprExpr (IntermediateTypeWitnessExprExpr tWitness)) = generateTWitness tWitness
generateExpression (IntermediateExprExpr expr@(IntermediateScopeBinding _)) = error $ "IntermediateScopeBinding should never be present when generating Aiken: " ++ show expr

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
generateNameAccessChain :: NameAccessChain -> Text
generateNameAccessChain (LocalNameAccessChain ident) = packIdent ident
generateNameAccessChain (AliasedNameAccessChain addr ident) =
  packAddress addr <> pack "." <> packIdent ident
-- Note that in Aiken, modules are referred just by their name
generateNameAccessChain (UnaliasedNameAccessChain _addr moduleIdent ident) =
  packIdent moduleIdent <> pack "." <> packIdent ident

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

-- |
-- Maps an Address to Text
--
-- An address is always mapped as a Byterray string
packAddress :: Address -> Text
packAddress (NamedAddress ident) = packStringify $ packIdent ident
packAddress (NumericalAddress (LiteralIntDec val)) = packStringify $ pack (show val)
packAddress (NumericalAddress (LiteralIntHex val)) = packStringify $ pack val

-- |
-- Maps a Type Witness to Text
generateTWitness :: IntermediateTypeWitnessExpr -> Text
generateTWitness (IntermediateTypeWithnessC nac tWitnessArgs) =
  [trimming|
    TypeWithnessC($nac', [$tWitnessArgs'])
  |]
  where
    nac' = packStringify $ generateNameAccessChain nac
    tWitnessArgs' = intercalate (pack ", ") $ map generateTWitness tWitnessArgs
generateTWitness (IntermediateTypeWitnessIdent ident) = packIdent ident

-- |
-- Given any Text, encloses it in string apices like "text"
packStringify :: Text -> Text
packStringify txt = pack "\"" <> txt <> pack "\""