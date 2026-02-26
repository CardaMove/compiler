module Move.Translations.PostProcessing (postProcessRoot, scopeUtilsIdent, refUtilsIdent, gsUtilsIdent) where

import Data.Generics.Uniplate.Data (transformBi)
import Data.Map qualified as Map
import Move.AST
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.Utils (utilsLibAddress)

-- |
-- Performs some post processing on the AST before generating the Aiken code.
--
-- Adds necessary imports for the custom Aiken library
--
-- Resolves named addresses into numerical addresses.
-- If a named address other than the utils is not resolved, an error is thrown
postProcessRoot :: Root -> AddrAssociations -> Root
postProcessRoot root addrAssociations =
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

-- |
-- The imports to add to all modules, consisting in a custom Aiken library
stdLibUses :: [TopLevel]
stdLibUses =
  [ TopLevelUse $
      Use
        { useAddress = utilsLibAddress,
          useIdentifier = Identifier "common_utils",
          useAlias = Nothing,
          useMembers =
            [ UseMember {useMemberIdentifier = Identifier "CPS", useMemberUseAlias = Nothing},
              UseMember {useMemberIdentifier = Identifier "Reference", useMemberUseAlias = Nothing},
              UseMember {useMemberIdentifier = Identifier "Addr", useMemberUseAlias = Nothing},
              UseMember {useMemberIdentifier = Identifier "TWitness", useMemberUseAlias = Nothing},
              UseMember {useMemberIdentifier = Identifier "TypeWithnessC", useMemberUseAlias = Nothing}
            ]
        },
    TopLevelUse $ Use {useAddress = NamedAddress $ Identifier "utils", useIdentifier = scopeUtilsIdent, useAlias = Nothing, useMembers = []},
    TopLevelUse $ Use {useAddress = NamedAddress $ Identifier "utils", useIdentifier = refUtilsIdent, useAlias = Nothing, useMembers = []},
    TopLevelUse $ Use {useAddress = NamedAddress $ Identifier "utils", useIdentifier = gsUtilsIdent, useAlias = Nothing, useMembers = []},
    TopLevelUse $
      Use
        { useAddress = NamedAddress $ Identifier "utils",
          useIdentifier = Identifier "signer_utils",
          useAlias = Nothing,
          useMembers =
            [ UseMember {useMemberIdentifier = Identifier "Signer", useMemberUseAlias = Nothing}
            ]
        }
  ]

-- |
-- Name of the custom Aiken library for scopes
scopeUtilsIdent :: Identifier
scopeUtilsIdent = Identifier "scope_utils"

-- |
-- Name of the custom Aiken library for references
refUtilsIdent :: Identifier
refUtilsIdent = Identifier "reference_utils"

-- |
-- Name of the custom Aiken library for global storage
gsUtilsIdent :: Identifier
gsUtilsIdent = Identifier "global_storage_utils"