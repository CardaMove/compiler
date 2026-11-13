module Move.Translations.SpecBlocks (stripSpecBlocksInRoot) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.Set qualified as Set
import Move.AST

-- |
-- Removes the MSL spec blocks from a root, together with the imports that only they use, so that the
-- Move runtime pipeline can run on spec-annotated sources (doc/msl.md, step 2).
-- Imports that the Move code uses, or that nothing uses, are left untouched.
stripSpecBlocksInRoot :: Root -> Root
stripSpecBlocksInRoot (RModule m@Module {moduleTopLevels}) = RModule m {moduleTopLevels = stripSpecBlocks moduleTopLevels}
stripSpecBlocksInRoot (RScript s@Script {scriptTopLevels}) = RScript s {scriptTopLevels = stripSpecBlocks scriptTopLevels}

-- |
-- Drops the spec blocks among the top levels, then the `use` declarations (or single use members)
-- that are referenced by some spec block but by no other top level.
stripSpecBlocks :: [TopLevel] -> [TopLevel]
stripSpecBlocks topLevels = concatMap stripTopLevel topLevels
  where
    specBlocks = [specBlock | TopLevelSpecBlock specBlock <- topLevels]
    codeTopLevels = filter isCode topLevels

    usedInSpecs = Set.fromList (universeBi specBlocks :: [NameAccessChain])
    usedInCode = Set.fromList (universeBi codeTopLevels :: [NameAccessChain])

    -- A set of name access chains is spec-only if the specs use some of them and the code none of them
    isSpecOnly :: [NameAccessChain] -> Bool
    isSpecOnly nacs = any (`Set.member` usedInSpecs) nacs && not (any (`Set.member` usedInCode) nacs)

    stripTopLevel :: TopLevel -> [TopLevel]
    stripTopLevel (TopLevelSpecBlock _) = []
    stripTopLevel (TopLevelUse use)
      | isSpecOnly (useNacs use) = []
      | otherwise = [TopLevelUse use {useMembers = filter (not . isSpecOnly . useMemberNacs) (useMembers use)}]
    stripTopLevel topLevel = [topLevel]

    -- Every name access chain the code can reference through a `use`: its module qualifiers and its members
    useNacs :: Use -> [NameAccessChain]
    useNacs use = filter (usesModule use) (Set.toList (Set.union usedInSpecs usedInCode)) ++ concatMap useMemberNacs (useMembers use)

    -- Whether a name access chain is qualified by the module imported by a `use` (also when the `use` imports `Self`)
    usesModule :: Use -> NameAccessChain -> Bool
    usesModule Use {useIdentifier, useAlias} (AliasedNameAccessChain qualifier _) = qualifier `elem` (useIdentifier : maybe [] pure useAlias)
    usesModule Use {useAddress, useIdentifier} (UnaliasedNameAccessChain address ident _) = address == useAddress && ident == useIdentifier
    usesModule _ (LocalNameAccessChain _) = False

    -- The local names a use member is reachable with. `Self` brings in only the module qualifier, handled by `usesModule`
    useMemberNacs :: UseMember -> [NameAccessChain]
    useMemberNacs UseMember {useMemberIdentifier = Identifier "Self"} = []
    useMemberNacs UseMember {useMemberIdentifier, useMemberUseAlias} = map LocalNameAccessChain (useMemberIdentifier : maybe [] pure useMemberUseAlias)

-- |
-- Whether a top level belongs to the Move code, i.e. it is neither a spec block nor a `use` declaration.
-- Uses are excluded since they reference no name access chain, and only the code matters to decide which to keep.
isCode :: TopLevel -> Bool
isCode (TopLevelSpecBlock _) = False
isCode (TopLevelUse _) = False
isCode _ = True
