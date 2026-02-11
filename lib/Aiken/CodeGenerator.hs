{-# LANGUAGE QuasiQuotes #-}

module Aiken.CodeGenerator where

import Data.Text (Text, empty, intercalate, null, pack)
import Move.AST
import Move.Translations.Utils (unitType)
import NeatInterpolation (trimming)

-- |
-- Given either a Script or a Module, generates its corresponding Aiken code
--
-- NOTE: the default indentation size for Aiken is two spaces
generateRoot :: Root -> Text
generateRoot (RModule Module {moduleTopLevels}) =
  join "\n" $ filter (not . Data.Text.null) $ map generateTopLevel moduleTopLevels
generateRoot (RScript Script {scriptTopLevels}) =
  join "\n" $ filter (not . Data.Text.null) $ map generateTopLevel scriptTopLevels

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
generateTopLevel (TopLevelFunction Function {functionHasNativeModifier = True}) = error "Native functions are not supported"
generateTopLevel (TopLevelFunction Function {functionHasNativeModifier = False, functionVisibilityModifier, functionName, functionTypeParameters, functionParameters, functionReturnType, functionBody}) =
  [trimming|
    ${visibility}fn $name$tParams($fParams) -> $fReturn {
      $fBody
    }
  |]
  where
    visibility = case functionVisibilityModifier of
      Nothing -> empty
      _ -> pack "pub "

    name = packIdent functionName
    tParams = packTypeParams functionTypeParameters
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
-- TODO: Add support for Address and Signer, also for non local names
generateType :: Type -> Text
generateType (TypeConstructor (LocalNameAccessChain (Identifier "bool")) []) = pack "Bool"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u8")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u16")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u32")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u64")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u128")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "u256")) []) = pack "Int"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "address")) []) = error "Address type currently not supported"
generateType (TypeConstructor (LocalNameAccessChain (Identifier "signer")) []) = error "Signer type currently not supported"
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
-- The unit type () is translated to Void, like the unit expression
generateType (TypeTuple []) = pack "Void"
generateType _ = error "Unexpected Type"

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
-- The unit expression is translated as Void, like the unit type
generateExpression (CommaExpr []) = pack "Void"
generateExpression _ = error "Expressions are currently not supported"

-- |
-- Given a Move Sequence (not SequenceExpr), generates the corresponding Aiken code,
-- without braces '{' '}'. Braces should be handled both by `generateExpression` and top level function
-- TODO:
generateSequence :: Sequence -> Text
generateSequence _ = error "Sequences are currently not supported"