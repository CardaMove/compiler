module Move.Transpiler (transpiler) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad.State (runState)
import Move.AST (Root)
import Move.Translations.Loops (translateWhilesToFunctionsInRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID)

-- |
-- Tries to execute an IO operation, intercepting any error if thrown and providing additional informations
runStep :: String -> IO a -> IO a
runStep stepName step = do
  res <- try step
  case res of
    Left err -> error $ "Transpiling failed at step " ++ stepName ++ ": " ++ displayException (err :: ErrorCall)
    Right val -> pure val

-- |
-- Given the whole parsed ASTs, translates them
--
-- TODO: For now, each AST is translated independently from each other
transpiler :: [Root] -> IO [Root]
transpiler = mapM transpileRoot

-- |
-- Translates a single AST root alone
transpileRoot :: Root -> IO Root
transpileRoot root = do
  step1NoLoops <- runStep "translateWhilesToFunctionsInRoot" $ evaluate $ translateWhilesToFunctionsInRoot root

  (step2Annotated, uuidAfterAnnotations) <-
    runStep "annotateBindingsWithUUID" $ evaluate $ runState (annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <-
    runStep "translateTParamsInRoot" $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- runStep "markVariablesForLocalScope" $ evaluate $ markVariablesForLocalScope step3TypeWitness

  runStep "addLocalScopeInRoot" $ evaluate $ addLocalScopeInRoot step3TypeWitness markedVars uuidAfterTypeWitness
