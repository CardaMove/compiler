{-# LANGUAGE QuasiQuotes #-}

module Aiken.ValidatorGenerator (
  ScriptMainMetadata (..),
  scriptMainParameters,
  collectScriptMainMetadata,
  generateValidator
) where

import Aiken.AikenGenerator (generateType, generateTopLevel)
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.List (foldl')
import Data.Text (Text, intercalate, pack)
import Move.AST
import NeatInterpolation (trimming)
import System.FilePath (takeBaseName)
import Move.Translations.Utils (stdLibUses)

-- | Metadata needed to generate the project-level validator.
data ScriptMainMetadata = ScriptMainMetadata
  { scriptConstructorName :: Identifier,
    scriptModuleName :: Identifier,
    scriptImportPath :: FilePath,
    scriptFunctionName :: Identifier,
    scriptMainParametersMeta :: [Parameter]
  }
  deriving (Eq, Show, Read)

data SignerParamKind = SignerValue | SignerRef
  deriving (Eq, Show)

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
scriptMainParameters = dropLast . functionParameters . scriptMainFunction
  where
    dropLast :: [a] -> [a]
    dropLast [] = []
    dropLast xs = init xs

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
              params = scriptMainParameters script
              tailData = collectScriptsMains rest
           in (filePath, function, params) : tailData
        _ -> collectScriptsMains rest

    -- | Maps extracted raw script information to validator metadata.
    buildMetas :: [(FilePath, Function, [Parameter])] -> [ScriptMainMetadata]
    buildMetas = map mk
      where
        mk :: (FilePath, Function, [Parameter]) -> ScriptMainMetadata
        mk (filePath, function, params) =
          let moduleStem = takeBaseName filePath
              moduleName = Identifier moduleStem
              importPath = "scripts/" ++ moduleStem
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
--
-- Since signers are inserted automatically by the Move VM, they need to be handled differently,
-- specifically, they will not result in the Redeemer type but rether will be retrieved from the
-- function signatories
generateValidator :: [ScriptMainMetadata] -> Text
generateValidator scriptsMeta =
  [trimming|
    use cardano/transaction.{OutputReference, Transaction}
    use aiken/collection/list
    use aiken/collection/dict.{Dict}
    use cardano/assets.{Value}

    $stdLibImports

    use utils/state_utils.{SerializedProgramState}
    use utils/assets_utils
    use utils/coin_utils
    use utils/common_utils.{ProgramState}

    $imports

    pub type Redeemer {
      $redeemerVariants
    }

    validator cardamove_validator {
      spend(
        datum: Option<SerializedProgramState>,
        redeemer: Redeemer,
        utxo: OutputReference,
        self: Transaction,
      ) {
        // At the start of the validator:

        // 1- Retrieve assets in input and outputs. Outputs will have the fees automatically deducted
        let tx_input_assets: Dict<Addr, Value> = list.map(self.inputs, fn (input) { input.output })
          |> assets_utils.map_outputs_to_assets()

        let tx_output_assets: Dict<Addr, Value> = self.outputs
          |> assets_utils.map_outputs_to_assets()

        // 2- Compute the initial state and the state in the tx output
        let program_state: ProgramState = state_utils.get_tx_input_state(datum)
          |> coin_utils.add_assets_to_state(tx_input_assets)

        let output_state: ProgramState = state_utils.get_tx_output_state(utxo, self)

        let cps: CPS = ([], program_state)

        // Pattern matchin the Redeemer
        let (_, program_state): CPS = when redeemer is {
          $spendBranches
        }

        // At the end of the validator:

        // 1- Get the assets that are expected to be present as output, deducing the fees from them
        // Additionally, the balance of the contract is removed. This is because it is not trivial to track when an asset is added to the contract
        // (TODO: will still be needed to be done in Mesh) so at least for now, it is not checked. This is still secure since only one address is not checked
        let contract_address: Addr = common_utils.get_contract_address(utxo, self)
          |> common_utils.parse_contract_address()

        let program_assets: Dict<Addr, Value> = program_state
          |> coin_utils.get_assets_from_state()
          |> assets_utils.deduce_transaction_fees_from_assets(signer_utils.get_tx_signer(self), self.fee)
          |> dict.delete(contract_address)

        let tx_output_assets = tx_output_assets
          |> dict.delete(contract_address)

        // 2- Remove from the program state the intermediate values used to keep track of the assets
        // Or from addresses that have no more state
        let program_state = program_state
          |> coin_utils.remove_assets_from_state()
          |> state_utils.purge_state()

        // 3- Make final comparisons
        and {
          // Compare program_state with output_state
          state_utils.compare_tx_states(program_state, output_state),
          // Compare expected_assets with tx output assets
          assets_utils.compare_assets(program_assets, tx_output_assets)
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
    stdLibImports = joinLines $ map generateTopLevel stdLibUses
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
      case filter (not . isSignerParam) scriptMainParametersMeta of
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
      case scriptMainParametersMeta of
        [] ->
          [trimming|
            $ctor' -> {
              let (_, cps) = $moduleName'.$functionName'(cps)
              cps
            }
          |]
        params ->
          let signerKinds = collectSignerKinds params
              signerCount = length signerKinds
              redeemerCount = length params - signerCount
              patternArgs = intercalate (pack ", ") $ map (pack . redeemerArgName signerCount) [0 .. redeemerCount - 1]
              callArgs = intercalate (pack ", ") $ buildCallArgs params signerCount signerKinds
              signerBindings = joinLines $ map mkSignerBinding (zip [0 ..] signerKinds)
           in if redeemerCount == 0
                then
                  [trimming|
                    $ctor' -> {
                      $signerBindings
                      let (_, cps) = $moduleName'.$functionName'($callArgs, cps)
                      cps
                    }
                  |]
                else
                  [trimming|
                    $ctor'($patternArgs) -> {
                      $signerBindings
                      let (_, cps) = $moduleName'.$functionName'($callArgs, cps)
                      cps
                    }
                  |]
      where
        ctor' = pack ctor
        moduleName' = pack moduleName
        functionName' = pack functionName

    -- Generates the code used to extract each i-th signer from the transaction signatories
    --
    -- Depending if the signer is a reference or not, it might be needed to push it on the scope and reference it
    mkSignerBinding :: (Int, SignerParamKind) -> Text
    mkSignerBinding (idx, kind) =
      case kind of
        SignerValue ->
          [trimming|
            expect Some($argName') = list.at(self.extra_signatories, $idxText)
            let $argName' = Signer{address: $argName'}
          |]
        SignerRef ->
          [trimming|
            expect Some($argName') = list.at(self.extra_signatories, $idxText)
            let $argName' = Signer{address: $argName'}
            let cps = scope_utils.post_scope("$argIdent'", $argName', cps)
          |]
      where
        argIdent = signerArgName idx
        argIdent' = pack argIdent
        argName' = pack argIdent
        idxText = pack $ show idx

    signerArgName :: Int -> String
    signerArgName i = "arg" ++ show i

    redeemerArgName :: Int -> Int -> String
    redeemerArgName signerCount i = "arg" ++ show (i + signerCount)

    -- Builds a function argument, handling the specific case for signers and references to signers
    buildCallArgs :: [Parameter] -> Int -> [SignerParamKind] -> [Text]
    buildCallArgs params signerCount signerKinds =
      let (args, _, _) = foldl' step ([], 0, 0) params
       in reverse args
      where
        step (acc, signerIdx, redeemerIdx) param
          | isSignerParam param =
              let argName = signerArgName signerIdx
                  argNameText = pack argName
                  callArg = case signerKinds !! signerIdx of
                    SignerValue -> argNameText
                    SignerRef -> [trimming|reference_utils.make_ref("$argNameText", [], cps)|]
               in (callArg : acc, signerIdx + 1, redeemerIdx)
          | otherwise = (pack (redeemerArgName signerCount redeemerIdx) : acc, signerIdx, redeemerIdx + 1)

    -- | Joins generated lines with a newline separator.
    joinLines :: [Text] -> Text
    joinLines = intercalate (pack "\n")

    -- Used to check if a signer coming from an entry point signature is a reference or not
    collectSignerKinds :: [Parameter] -> [SignerParamKind]
    collectSignerKinds = foldr collect []
      where
        collect param acc = case signerParamKind param of
          Just kind -> kind : acc
          Nothing -> acc

    -- Check if a certain parameter of an entry point function is a signer, either reference or not
    isSignerParam :: Parameter -> Bool
    isSignerParam = maybe False (const True) . signerParamKind

    signerParamKind :: Parameter -> Maybe SignerParamKind
    signerParamKind Parameter {parameterType} = signerTypeKind parameterType

    signerTypeKind :: Type -> Maybe SignerParamKind
    signerTypeKind (TypeImmutableRef inner) =
      case signerTypeKind inner of
        Just _ -> Just SignerRef
        Nothing -> Nothing
    signerTypeKind (TypeMutableRef inner) =
      case signerTypeKind inner of
        Just _ -> Just SignerRef
        Nothing -> Nothing
    signerTypeKind (TypeConstructor nac []) =
      case nac of
        LocalNameAccessChain (Identifier "signer") -> Just SignerValue
        AliasedNameAccessChain _ (Identifier "signer") -> Just SignerValue
        UnaliasedNameAccessChain _ _ (Identifier "signer") -> Just SignerValue
        _ -> Nothing
    signerTypeKind _ = Nothing

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
