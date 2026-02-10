module Move.Loader where

import Control.Exception (ErrorCall, Exception (displayException), evaluate, try)
import Move.AST (Root)
import Move.Lexer (scan)
import Move.Parser (parse)

-- |
-- Tries to execute an IO operation, intercepting any error if thrown and providing additional informations
runStep :: String -> IO a -> IO a
runStep stepName step = do
  res <- try step
  case res of
    Left err -> error $ "Loading failed at step " ++ stepName ++ ": " ++ displayException (err :: ErrorCall)
    Right val -> pure val

-- |
-- Loads a single file and parses it
loadFile :: String -> IO Root
loadFile path = do
  origin <- runStep ("File loading: " ++ show path) $ readFile path
  runStep ("File parsing: " ++ show path) $ evaluate $ parse $ scan origin