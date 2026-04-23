module Move.Translations.PostProcessing (postProcessRoot) where

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
   in root''
