module Move.Transpiler (moveTranspiler, mslTranspiler, offchainTranspiler) where

import Control.Exception (evaluate)
import Control.Monad.State (evalState, runState)
import Move.AST (Root)
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.AlphaConversion (alphaConvert)
import Move.Translations.Loops (translateWhilesToFunctionsInRoot)
import Move.Translations.Polymorphism (translatePolymorphismInRoot)
import Move.Translations.PostProcessing (postProcessRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.SpecBlocks (stripSpecBlocksInRoot)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID, tryIO)
import Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks, stripMoveFunctions)
import Move.TranslationsMSL.SpecInlining (inlineSpecSchemas)

-- |
-- Given the whole parsed ASTs, translates them for the Move runtime on-chain mode and for the off-chain target.
-- The MSL spec blocks, and the imports only they use, are dropped first: this chain compiles the Move code only
moveTranspiler :: [(FilePath, Root)] -> AddrAssociations -> IO [(FilePath, Root)]
moveTranspiler files addrAssoc = do
  allRoots <- tryIO "stripSpecBlocksInRoot" $ evaluate $ map (stripSpecBlocksInRoot . snd) files
  -- FIXME: Also invoke `translateLoopsToWhile`
  step1NoLoops <- tryIO "translateWhilesToFunctionsInRoot" $ evaluate $ translateWhilesToFunctionsInRoot allRoots

  (step2Annotated, uuidAfterAnnotations) <- tryIO "annotateBindingsWithUUID" $ evaluate $ runState (mapM annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <- tryIO "translateTParamsInRoot" $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- tryIO "markVariablesForLocalScope" $ evaluate $ markVariablesForLocalScope step3TypeWitness

  (step4LocalScope, uuidAfterLocalScope) <- tryIO "addLocalScopeInRoot" $ evaluate $ runState (addLocalScopeInRoot step3TypeWitness markedVars) uuidAfterTypeWitness

  step5Polymorphism <- tryIO "translatePolymorphismInRoot" $ evaluate $ evalState (translatePolymorphismInRoot step4LocalScope) uuidAfterLocalScope

  let allFileNames = map fst files

  tryIO "postProcessing" $ evaluate $ zipWith (\fileName root -> (fileName, postProcessRoot root addrAssoc)) allFileNames step5Polymorphism

-- |
-- Given the whole parsed ASTs, translates them for the MSL on-chain mode (the default `--onchain-mode` is `move`).
-- Not implemented yet: it runs the passes written so far (`annotateSpecBlocks`, `stripMoveFunctions`,
-- `inlineSpecSchemas`, `annotateBindingsWithUUID`), then always fails, since the MSL validator generation
-- does not exist yet (step 6 of doc/msl.md); its signature is a placeholder until then
mslTranspiler :: [(FilePath, Root)] -> AddrAssociations -> IO [(FilePath, Root)]
mslTranspiler files _addrAssoc = do
  -- Each step is fully evaluated (via `show`) so that its errors surface before the final failure
  step1Annotated <- tryIO "annotateSpecBlocks" $ forceRoots $ map (annotateSpecBlocks . snd) files

  step2NoFunctions <- tryIO "stripMoveFunctions" $ forceRoots $ stripMoveFunctions step1Annotated

  step3Inlined <- tryIO "inlineSpecSchemas" $ forceRoots $ inlineSpecSchemas step2NoFunctions

  _step4UUIDs <- tryIO "annotateBindingsWithUUID" $ forceRoots $ evalState (mapM annotateBindingsWithUUID step3Inlined) 0

  ioError $ userError "The MSL on-chain mode is not implemented yet: use `--onchain-mode move`"
  where
    -- Fully evaluates the given roots (by printing them) and returns them
    forceRoots :: [Root] -> IO [Root]
    forceRoots roots = evaluate (length (show roots)) >> return roots

-- |
-- Given the output of `moveTranspiler`, applies the passes needed only by the off-chain (TypeScript) target.
-- `alphaConvert` must run after `postProcessRoot`, since TypeScript can not rebind a variable in the same scope
offchainTranspiler :: [(FilePath, Root)] -> IO [(FilePath, Root)]
offchainTranspiler files = do
  converted <- tryIO "alphaConvert" $ evaluate $ alphaConvert (map snd files)
  return $ zip (map fst files) converted
