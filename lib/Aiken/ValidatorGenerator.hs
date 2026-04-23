{-# LANGUAGE QuasiQuotes #-}

module Aiken.ValidatorGenerator (
  ScriptMainMetadata (..),
  scriptMainParameters,
  collectScriptMainMetadata,
  generateValidator
) where

import Aiken.AikenGenerator (generateType)
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Text (Text, intercalate, pack)
import Move.AST
import NeatInterpolation (trimming)
import System.FilePath (takeBaseName)

-- | Metadata needed to generate the project-level validator.
data ScriptMainMetadata = ScriptMainMetadata
  { scriptConstructorName :: Identifier,
    scriptModuleName :: Identifier,
    scriptImportPath :: FilePath,
    scriptFunctionName :: Identifier,
    scriptMainParametersMeta :: [Parameter]
  }
  deriving (Eq, Show, Read)

-- | Given a Script, extracts the only function it contains.
--
-- Fails if there is no function or if multiple functions are found.
scriptMainFunction :: Script -> Function
scriptMainFunction Script {scriptTopLevels} =
  case [function | TopLevelFunction function <- scriptTopLevels] of
    [function] -> function
    [] -> error "Expected exactly one function in script, found none"
    _ -> error "Expected exactly one function in script, found multiple"

-- | Given a Script, extracts parameters accepted by its only function.
scriptMainParameters :: Script -> [Parameter]
scriptMainParameters = functionParameters . scriptMainFunction

-- | Collects metadata for all transpiled scripts.
--
-- This function filters script roots, extracts each script `main` signature,
-- and computes constructor/import information used by `generateValidator`.
collectScriptMainMetadata :: [(FilePath, Root)] -> [ScriptMainMetadata]
collectScriptMainMetadata files =
  let raw = collectScriptsMains files
   in buildMetas raw
  where
    -- | Extracts `(sourceFilePath, mainParameters)` from script roots only.
    collectScriptsMains :: [(FilePath, Root)] -> [(FilePath, Function, [Parameter])]
    collectScriptsMains [] = []
    collectScriptsMains ((filePath, root) : rest) =
      case root of
        RScript script ->
          let function = scriptMainFunction script
              params = functionParameters function
              tailData = collectScriptsMains rest
           in (filePath, function, params) : tailData
        _ -> collectScriptsMains rest

    -- | Maps extracted raw script information to validator metadata.
    buildMetas :: [(FilePath, Function, [Parameter])] -> [ScriptMainMetadata]
    buildMetas = map mk
      where
        mk :: (FilePath, Function, [Parameter]) -> ScriptMainMetadata
        mk (filePath, function, params) =
          let moduleName = Identifier $ takeBaseName filePath
              importPath = "scripts/" ++ takeBaseName filePath
              ctorName = constructorBase moduleName
           in ScriptMainMetadata
                { scriptConstructorName = ctorName,
                  scriptModuleName = moduleName,
                  scriptImportPath = importPath,
                  scriptFunctionName = functionName function,
                  scriptMainParametersMeta = params
                }

    -- | Builds the Redeemer constructor name from the script module name.
    --
    -- Example: `script-user` becomes `ScriptUser`.
    constructorBase :: Identifier -> Identifier
    constructorBase (Identifier base) =
      let
        tokens = tokenizeAlphaNum base
        pascal = concatMap toPascal tokens
       in Identifier $ if null pascal then "Script" else pascal

-- | Generates the full `validator.ak` source text.
--
-- Output includes imports for script modules, the `Redeemer` type,
-- and a `spend` matcher that dispatches to the corresponding script `main`.
generateValidator :: [ScriptMainMetadata] -> Text
generateValidator scriptsMeta =
  [trimming|
    $imports

    pub type Redeemer {
      $redeemerVariants
    }

    validator cardamove_validator {
      spend(_datum, redeemer: Redeemer, _own_ref, _tx) {
        when redeemer is {
          $spendBranches
        }
      }

      mint(_redeemer, _policy_id, _tx) {
        False
      }

      withdraw(_redeemer, _credential, _tx) {
        False
      }
    }
  |]
  where
    imports = joinLines $ map mkImport scriptsMeta
    redeemerVariants = joinLines $ map mkRedeemerVariant scriptsMeta
    spendBranches = joinLines $ map mkSpendBranch scriptsMeta

    -- | Generates one import line for a script module.
    mkImport :: ScriptMainMetadata -> Text
    mkImport ScriptMainMetadata {scriptImportPath} =
      [trimming|
        use $path
      |]
      where
        path = pack scriptImportPath

    -- | Generates one `Redeemer` constructor variant.
    mkRedeemerVariant :: ScriptMainMetadata -> Text
    mkRedeemerVariant ScriptMainMetadata {scriptConstructorName = Identifier ctor, scriptMainParametersMeta} =
      case scriptMainParametersMeta of
        [] -> pack ctor
        params ->
          [trimming|
            $ctor'($args)
          |]
          where
            ctor' = pack ctor
            args = intercalate (pack ", ") $ map (generateType . parameterType) params

    -- | Generates one `spend` pattern-match branch that dispatches to `main`.
    mkSpendBranch :: ScriptMainMetadata -> Text
    mkSpendBranch ScriptMainMetadata {scriptConstructorName = Identifier ctor, scriptModuleName = Identifier moduleName, scriptFunctionName = Identifier functionName, scriptMainParametersMeta} =
      case zip [0 :: Int ..] scriptMainParametersMeta of
        [] ->
          [trimming|
            $ctor' -> {
              let _ = $moduleName'.$functionName'()
              True
            }
          |]
        indexedParams ->
          [trimming|
            $ctor'($patternArgs) -> {
              let _ = $moduleName'.$functionName'($callArgs)
              True
            }
          |]
          where
            argName i = "arg" ++ show i
            patternArgs = intercalate (pack ", ") [pack (argName idx) | (idx, _) <- indexedParams]
            callArgs = intercalate (pack ", ") [pack (argName idx) | (idx, _) <- indexedParams]
      where
        ctor' = pack ctor
        moduleName' = pack moduleName
        functionName' = pack functionName

    -- | Joins generated lines with a newline separator.
    joinLines :: [Text] -> Text
    joinLines = intercalate (pack "\n")

-- | Splits a string into contiguous alphanumeric tokens.
--
-- Example: `script-user_0` becomes `["script", "user", "0"]`.
tokenizeAlphaNum :: String -> [String]
tokenizeAlphaNum = go [] []
  where
    go :: [String] -> String -> String -> [String]
    go acc curr [] =
      if null curr
        then reverse acc
        else reverse (reverse curr : acc)
    go acc curr (ch : rest)
      | isAlphaNum ch = go acc (ch : curr) rest
      | null curr = go acc [] rest
      | otherwise = go (reverse curr : acc) [] rest

-- | Converts a token to PascalCase.
toPascal :: String -> String
toPascal [] = []
toPascal (ch : rest) = toUpper ch : map toLower rest
