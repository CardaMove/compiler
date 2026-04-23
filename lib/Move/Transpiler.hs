module Move.Transpiler (transpiler) where

import Control.Exception (evaluate)
import Control.Monad.State (evalState, runState)
import Move.AST (Root)
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.Loops (translateWhilesToFunctionsInRoot)
import Move.Translations.Polymorphism (translatePolymorphismInRoot)
import Move.Translations.PostProcessing (postProcessRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID, tryIO)

-- |
-- Given the whole parsed ASTs, translates them
transpiler :: [(FilePath, Root)] -> AddrAssociations -> IO [(FilePath, Root)]
transpiler files addrAssoc = do
  let allRoots = map snd files
  -- FIXME: Also invoke `translateLoopsToWhile`
  step1NoLoops <- tryIO "translateWhilesToFunctionsInRoot" $ evaluate $ translateWhilesToFunctionsInRoot allRoots

  (step2Annotated, uuidAfterAnnotations) <- tryIO "annotateBindingsWithUUID" $ evaluate $ runState (mapM annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <- tryIO "translateTParamsInRoot" $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- tryIO "markVariablesForLocalScope" $ evaluate $ markVariablesForLocalScope step3TypeWitness

  (step4LocalScope, uuidAfterLocalScope) <- tryIO "addLocalScopeInRoot" $ evaluate $ runState (addLocalScopeInRoot step3TypeWitness markedVars) uuidAfterTypeWitness

  step5Polymorphism <- tryIO "translatePolymorphismInRoot" $ evaluate $ evalState (translatePolymorphismInRoot step4LocalScope) uuidAfterLocalScope

  let allFileNames = map fst files

  tryIO "postProcessing" $ evaluate $ zipWith (\fileName root -> (fileName, postProcessRoot root addrAssoc)) allFileNames step5Polymorphism
