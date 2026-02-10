module Move.Loader.Loader where

import Control.Exception (ErrorCall, Exception (displayException), evaluate, try)
import Move.AST (Root)
import Move.Lexer (scan)
import Move.Parser (parse)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

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
  file <- runStep ("File loading: " ++ show path) $ readFile path
  runStep ("File parsing: " ++ show path) $ evaluate $ parse $ scan file

-- |
-- Given the path to a `Move.toml` file, loads the entire project files
loadToml :: String -> IO [Root]
loadToml tomlPath = do
  tomlExists <- doesFileExist tomlPath
  if tomlExists
    then do
      -- Loads all files inside `./scripts` and `./sources`
      scripts <- loadFolder "scripts"
      sources <- loadFolder "sources"

      pure $ scripts ++ sources
    else error $ "Move.toml file not found at: " ++ show tomlPath
  where
    loadFolder :: String -> IO [Root]
    loadFolder folderName = do
      let dirPath = takeDirectory tomlPath </> folderName
      filesInDir <- listDirectory dirPath
      mapM (loadFile . (dirPath </>)) $ filter ((== ".move") . takeExtension) filesInDir