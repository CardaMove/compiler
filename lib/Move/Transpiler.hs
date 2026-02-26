module Move.Transpiler (transpiler) where

import Control.Exception (evaluate)
import Control.Monad.State (runState)
import Move.AST (Root)
import Move.Loader.Loader (AddrAssociations)
import Move.Translations.Loops (translateWhilesToFunctionsInRoot)
import Move.Translations.PostProcessing (postProcessRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID, tryIO)

-- |
-- Given the whole parsed ASTs, translates them
--
-- TODO: For now, each AST is translated independently from each other
transpiler :: [(FilePath, Root)] -> AddrAssociations -> IO [(FilePath, Root)]
transpiler files addrAssoc = mapM (\(f, r) -> transpileRoot f r addrAssoc) files

-- |
-- Translates a single AST root alone
transpileRoot :: FilePath -> Root -> AddrAssociations -> IO (FilePath, Root)
transpileRoot fileName root addrAssoc = do
  step1NoLoops <- tryIO (fileName ++ ": translateWhilesToFunctionsInRoot") $ evaluate $ translateWhilesToFunctionsInRoot root

  (step2Annotated, uuidAfterAnnotations) <-
    tryIO (fileName ++ ": annotateBindingsWithUUID") $ evaluate $ runState (annotateBindingsWithUUID step1NoLoops) 0

  (step3TypeWitness, uuidAfterTypeWitness) <-
    tryIO (fileName ++ ": translateTParamsInRoot") $ evaluate $ runState (translateTParamsInRoot step2Annotated) uuidAfterAnnotations

  markedVars <- tryIO (fileName ++ ": markVariablesForLocalScope") $ evaluate $ markVariablesForLocalScope step3TypeWitness

  step4LocalScope <- tryIO (fileName ++ ": addLocalScopeInRoot") $ evaluate $ addLocalScopeInRoot step3TypeWitness markedVars uuidAfterTypeWitness

  tryIO (fileName ++ ": postProcessing") $ evaluate (fileName, postProcessRoot step4LocalScope addrAssoc)
