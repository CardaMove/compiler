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
-- Not implemented yet: it always fails. Its passes and its signature are designed in steps 4-5 of doc/msl.md
mslTranspiler :: [(FilePath, Root)] -> AddrAssociations -> IO [(FilePath, Root)]
mslTranspiler _files _addrAssoc =
  ioError $ userError "The MSL on-chain mode is not implemented yet: use `--onchain-mode move`"

-- |
-- Given the output of `moveTranspiler`, applies the passes needed only by the off-chain (TypeScript) target.
-- `alphaConvert` must run after `postProcessRoot`, since TypeScript can not rebind a variable in the same scope
offchainTranspiler :: [(FilePath, Root)] -> IO [(FilePath, Root)]
offchainTranspiler files = do
  converted <- tryIO "alphaConvert" $ evaluate $ alphaConvert (map snd files)
  return $ zip (map fst files) converted
