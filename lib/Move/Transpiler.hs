module Move.Transpiler (transpiler) where

import Control.Exception (evaluate)
import Control.Monad.State (runState)
import Move.AST (Root)
import Move.Translations.Loops (translateWhilesToFunctionsInRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID, tryIO)
import Move.Translations.PostProcessing (postProcessRoot)

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
  step1NoLoops <- tryIO (fileName ++ ": translateWhilesToFunctionsInRoot") $ evaluate $ translateWhilesToFunctionsInRoot root

  (step2Annotated, uuidAfterAnnotations) <-
    tryIO (fileName ++ ": annotateBindingsWithUUID") $ evaluate $ runState (annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <-
    tryIO (fileName ++ ": translateTParamsInRoot") $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- tryIO (fileName ++ ": markVariablesForLocalScope") $ evaluate $ markVariablesForLocalScope step3TypeWitness

  step4LocalScope <- tryIO (fileName ++ ": addLocalScopeInRoot") $ evaluate $ addLocalScopeInRoot step3TypeWitness markedVars uuidAfterTypeWitness

  tryIO (fileName ++ ": postProcessing") $ evaluate (fileName, postProcessRoot step4LocalScope)
