module Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks) where

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
