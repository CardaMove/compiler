module Move.Translations.PostProcessing (postProcessRoot) where

import Data.Char (toLower)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Map qualified as Map
import Move.AST
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.Utils (utilsLibAddress, stdLibUses)

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
postProcessRoot :: Root -> AddrAssociations -> Root
postProcessRoot root addrAssociations =
  -- Note that this step does not need to consider other modules that might be imported,
  -- since it just substitutes certain AST nodes
  let root' = case root of
        (RModule m@Module {moduleTopLevels}) -> RModule $ m {moduleTopLevels = stdLibUses ++ moduleTopLevels}
        (RScript s@Script {scriptTopLevels}) -> RScript $ s {scriptTopLevels = stdLibUses ++ scriptTopLevels}

      root'' = transformBi resolveAddress root'
        where
          resolveAddress :: Address -> Address
          resolveAddress addr@(NamedAddress ident) =
            if addr == utilsLibAddress
              -- Leave the address for utils stdlib unchanged
              then addr
              else case Map.lookup ident addrAssociations of
                Nothing -> error $ "Unresolved named address: " ++ show addr
                Just num -> NumericalAddress num
          resolveAddress addr = addr

      root''' = transformBi normalizeModule $ transformBi normalizeUse $ transformBi normalizeUnaliasedNameAccessChain root''
   in root'''

-- | Normalizes a module by converting its address and identifier to lowercase.
normalizeModule :: Module -> Module
normalizeModule m@Module {moduleAddress, moduleIdentifier} =
  m
    { moduleAddress = lowercaseAddress moduleAddress,
      moduleIdentifier = lowercaseIdentifier moduleIdentifier
    }

-- | Normalizes a use statement by converting both the address and identifier to lowercase.
normalizeUse :: Use -> Use
normalizeUse u@Use {useAddress, useIdentifier} =
  u
    { useAddress = lowercaseAddress useAddress,
      useIdentifier = lowercaseIdentifier useIdentifier
    }

-- | Normalizes an unaliased name access chain by converting the first identifier to lowercase.
-- The second identifier and address are left unchanged.
normalizeUnaliasedNameAccessChain :: NameAccessChain -> NameAccessChain
normalizeUnaliasedNameAccessChain (UnaliasedNameAccessChain addr firstIdent secondIdent) =
  UnaliasedNameAccessChain addr (lowercaseIdentifier firstIdent) secondIdent
normalizeUnaliasedNameAccessChain chain = chain

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
