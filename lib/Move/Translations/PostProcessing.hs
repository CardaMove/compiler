module Move.Translations.PostProcessing (postProcessRoot, isStdSignerTopLevel) where

import Data.Char (toLower)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Move.AST
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.Utils (aptosFrameworkLibAddress, stdLibAddress, stdLibUses, utilsLibAddress)

-- |
-- Performs some post processing on the AST before generating the Aiken code.
--
-- Adds necessary imports for the custom Aiken library
--
-- Resolves named addresses into numerical addresses.
-- If a named address other than the utils is not resolved, an error is thrown
--
--
-- In addition, module named addresses and identifiers are converted to lowercase due to restrictions
-- imposed by the Aiken compiler
--
-- It also adds the "public entry" modifiers to the main function of each script, since by default Move does not explicitly require them
--
-- It also removes any top level `use` occurrence where the address is `std` and the identifier is `signer`,
-- since the signer module is handled by a custom Aiken library
postProcessRoot :: Root -> AddrAssociations -> Root
postProcessRoot root addrAssociations =
  -- Note that this step does not need to consider other modules that might be imported,
  -- since it just substitutes certain AST nodes
  let root' = case root of
        (RModule m@Module {moduleTopLevels}) -> RModule $ m {moduleTopLevels = stdLibUses ++ filter (not . isStdSignerTopLevel) moduleTopLevels}
        (RScript s@Script {scriptTopLevels}) -> RScript $ s {scriptTopLevels = stdLibUses ++ filter (not . isStdSignerTopLevel) scriptTopLevels}

      root'' = transformBi resolveAddress root'
        where
          resolveAddress :: Address -> Address
          resolveAddress addr@(NamedAddress ident) =
            if addr `elem` [utilsLibAddress, stdLibAddress, aptosFrameworkLibAddress]
              -- Leave the address for utils, Move stdlib and aptos framework unchanged
              then addr
              else case Map.lookup ident addrAssociations of
                Nothing -> error $ "Unresolved named address: " ++ show addr
                Just num -> NumericalAddress num
          resolveAddress addr = addr

      root''' = transformBi normalizeModule $ transformBi normalizeUse $ transformBi normalizeUnaliasedNameAccessChain root''

      root'''' = forceScriptFunctionsPublicEntry root'''
   in root''''

-- | Scripts in Move expose entry points implicitly; normalize their function modifiers accordingly.
forceScriptFunctionsPublicEntry :: Root -> Root
forceScriptFunctionsPublicEntry root = case root of
  RScript s@Script {scriptTopLevels} ->
    RScript $
      s
        { scriptTopLevels = map forceTopLevelFunctionPublicEntry scriptTopLevels
        }
  _ -> root

-- | Converts script top-level functions to `public entry`.
forceTopLevelFunctionPublicEntry :: TopLevel -> TopLevel
forceTopLevelFunctionPublicEntry topLevel = case topLevel of
  TopLevelFunction f ->
    TopLevelFunction
      f
        { functionVisibilityModifier = Just VisibilityModifierPublic,
          functionHasEntryModifier = True
        }
  _ -> topLevel

-- | Normalizes a module by converting its address and identifier to lowercase.
normalizeModule :: Module -> Module
normalizeModule m@Module {moduleAddress, moduleIdentifier} =
  m
    { moduleAddress = lowercaseAddress moduleAddress,
      moduleIdentifier = lowercaseIdentifier moduleIdentifier
    }

-- | Normalizes a use statement by converting both the address and identifier to lowercase.
--
-- In addition, the `use aptos_framework::coin as x` is rewritten as the utils module, but keeping the alias: `use utils/coin_utils as x`
-- if an alias x is not specified, it defaults to the original identifier of the module
normalizeUse :: Use -> Use
normalizeUse u@Use {useAddress, useIdentifier, useAlias} =
  let u' =
        u
          { useAddress = lowercaseAddress useAddress,
            useIdentifier = lowercaseIdentifier useIdentifier,
            useMembers = filter (\um -> useMemberIdentifier um /= Identifier "Self") (useMembers u)
          }
   in if useAddress == aptosFrameworkLibAddress
        then
          u' {useAddress = utilsLibAddress, useIdentifier = Identifier "coin_utils", useAlias = Just $ fromMaybe useIdentifier useAlias}
        else u'

-- | Normalizes an unaliased name access chain by converting the first identifier to lowercase.
-- The second identifier and address are left unchanged.
normalizeUnaliasedNameAccessChain :: NameAccessChain -> NameAccessChain
normalizeUnaliasedNameAccessChain (UnaliasedNameAccessChain addr firstIdent secondIdent) =
  UnaliasedNameAccessChain addr (lowercaseIdentifier firstIdent) secondIdent
normalizeUnaliasedNameAccessChain chain = chain

-- |
-- Checks if a top level is a `use std::signer`
isStdSignerTopLevel :: TopLevel -> Bool
isStdSignerTopLevel (TopLevelUse Use {useAddress = NamedAddress (Identifier "std"), useIdentifier = Identifier "signer"}) = True
isStdSignerTopLevel _ = False

-- | Converts an address to lowercase. Named addresses are lowercased, and hex numerals are lowercased.
lowercaseAddress :: Address -> Address
lowercaseAddress (NamedAddress ident) = NamedAddress $ lowercaseIdentifier ident
lowercaseAddress (NumericalAddress num) = NumericalAddress $ lowercaseNumerical num

-- | Converts a numerical value to lowercase. Only hex literals are affected; decimal values are unchanged.
lowercaseNumerical :: Numerical -> Numerical
lowercaseNumerical (LiteralIntDec val) = LiteralIntDec val
lowercaseNumerical (LiteralIntHex val) = LiteralIntHex $ map toLower val

-- | Converts an identifier string to lowercase for Aiken compatibility.
lowercaseIdentifier :: Identifier -> Identifier
lowercaseIdentifier (Identifier ident) = Identifier $ map toLower ident
