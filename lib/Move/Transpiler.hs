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
-- NOTE: The transpiler needs to know the (final) file paths, since needs to generate imports
-- quick note: imports in Move follow module name, while in Aiken follow file name, which has to be considered 
transpiler :: [(FilePath, Root)] -> IO [(FilePath, Root)]
transpiler = mapM $ uncurry transpileRoot

-- |
-- Translates a single AST root alone
transpileRoot :: FilePath -> Root -> IO (FilePath, Root)
transpileRoot fileName root = do
  step1NoLoops <- runStep (fileName ++ ": translateWhilesToFunctionsInRoot") $ evaluate $ translateWhilesToFunctionsInRoot root

  (step2Annotated, uuidAfterAnnotations) <-
    runStep (fileName ++ ": annotateBindingsWithUUID") $ evaluate $ runState (annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <-
    runStep (fileName ++ ": translateTParamsInRoot") $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- runStep (fileName ++ ": markVariablesForLocalScope") $ evaluate $ markVariablesForLocalScope step3TypeWitness

  runStep (fileName ++ ": addLocalScopeInRoot") $ evaluate (fileName, addLocalScopeInRoot step3TypeWitness markedVars uuidAfterTypeWitness)
