module Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks, stripMoveFunctions, stripUseMembers) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Move.AST

-- |
-- First pass of the MSL pipeline (`mslTranspiler`), to be run before any traversal of the AST.
--
-- Fills the `SpecBTFAnnotations` of each function spec block (`SpecBlockTargetFunction`) with the type parameters
-- and parameters of its target function, which must be declared in the same module or script, otherwise it is an error.
-- Spec schemas and every other top level are left untouched; existing annotations are overwritten
annotateSpecBlocks :: Root -> Root
annotateSpecBlocks (RModule rModule@Module {moduleTopLevels}) = RModule rModule {moduleTopLevels = annotateTopLevels moduleTopLevels}
annotateSpecBlocks (RScript rScript@Script {scriptTopLevels}) = RScript rScript {scriptTopLevels = annotateTopLevels scriptTopLevels}

-- |
-- Annotates the function spec blocks among the given top levels,
-- looking up their target functions among the same top levels
annotateTopLevels :: [TopLevel] -> [TopLevel]
annotateTopLevels topLevels = map annotateTopLevel topLevels
  where
    annotateTopLevel :: TopLevel -> TopLevel
    annotateTopLevel (TopLevelSpecBlock specBlock@SpecBlock {specBlockTarget = SpecBlockTargetFunction targetIdent _}) =
      case [f | TopLevelFunction f@Function {functionName} <- topLevels, functionName == targetIdent] of
        Function {functionTypeParameters, functionParameters} : _ ->
          let annotations = SpecBTFAnnotations {sbtpFunctionTypeParameters = functionTypeParameters, sbtfFunctionParameters = functionParameters}
           in TopLevelSpecBlock specBlock {specBlockTarget = SpecBlockTargetFunction targetIdent (Just annotations)}
        [] -> error $ "Spec block target function not found in the module: " ++ show targetIdent
    annotateTopLevel otherTopLevel = otherTopLevel

-- |
-- Second pass of the MSL pipeline, to be run after `annotateSpecBlocks` (which needs the target functions).
--
-- Given all the roots of the project, removes every function declaration (`TopLevelFunction`) from modules and scripts,
-- and the use members importing a function declared by one of the given modules (user-defined functions).
-- A use left with no members is removed; module imports (`use M;`, `Self`) and the imports of modules
-- that are not among the given roots (e.g. `std::signer`) are kept
stripMoveFunctions :: [Root] -> [Root]
stripMoveFunctions roots = map stripRoot roots
  where
    userModules = [m | RModule m <- roots]

    stripRoot :: Root -> Root
    stripRoot (RModule rModule@Module {moduleTopLevels}) = RModule rModule {moduleTopLevels = stripTopLevels moduleTopLevels}
    stripRoot (RScript rScript@Script {scriptTopLevels}) = RScript rScript {scriptTopLevels = stripTopLevels scriptTopLevels}

    -- Drops the functions and strips the uses, leaving the other top levels untouched
    stripTopLevels :: [TopLevel] -> [TopLevel]
    stripTopLevels = mapMaybe stripTopLevel
      where
        stripTopLevel (TopLevelFunction _) = Nothing
        stripTopLevel (TopLevelUse use) = TopLevelUse <$> stripUseMembers userModules functionNames use
        stripTopLevel otherTopLevel = Just otherTopLevel

    -- The names of the functions declared by a module
    functionNames :: Module -> [Identifier]
    functionNames Module {moduleTopLevels} = [functionName | TopLevelFunction Function {functionName} <- moduleTopLevels]

-- |
-- Given the modules of the project and a function returning some names declared by a module,
-- removes from a use the members that import one of those names from a project module.
-- Returns Nothing if no member is left (an empty member list would instead mean `use M;`).
-- Module imports without members and imports of modules that are not in the project are kept unchanged
stripUseMembers :: [Module] -> (Module -> [Identifier]) -> Use -> Maybe Use
stripUseMembers userModules namesToStrip use@Use {useAddress, useIdentifier, useMembers} =
  case find (\m -> moduleAddress m == useAddress && moduleIdentifier m == useIdentifier) userModules of
    -- Not a user module, or a module import without members: keep it
    Nothing -> Just use
    Just _ | null useMembers -> Just use
    Just importedModule ->
      let names = namesToStrip importedModule
          members' = filter (\UseMember {useMemberIdentifier} -> useMemberIdentifier `notElem` names) useMembers
       in if null members' then Nothing else Just use {useMembers = members'}
