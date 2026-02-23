module Move.Translations.PostProcessing (postProcessRoot, scopeUtilsIdent, refUtilsIdent, gsUtilsIdent) where

import Move.AST

-- |
-- Performs some post processing on the AST before generating the Aiken code.
--
-- Adds necessary imports for the custom Aiken library
postProcessRoot :: Root -> Root
postProcessRoot root =
  case root of
    (RModule m@Module {moduleTopLevels}) -> RModule $ m {moduleTopLevels = stdLibUses ++ moduleTopLevels}
    (RScript s@Script {scriptTopLevels}) -> RScript $ s {scriptTopLevels = stdLibUses ++ scriptTopLevels}

-- |
-- The imports to add to all modules, consisting in a custom Aiken library
stdLibUses :: [TopLevel]
stdLibUses =
  [ TopLevelUse $
      Use
        { useAddress = NamedAddress $ Identifier "utils",
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