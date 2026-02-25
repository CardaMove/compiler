module Move.Loader.Loader (loadToml) where

import Move.AST (Root)
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Utils (tryIO)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- |
-- Loads a single file and parses it
loadFile :: String -> IO (FilePath, Root)
loadFile path = do
  file <- tryIO ("File loading: " ++ show path) $ readFile path
  tryIO ("File parsing: " ++ show path) $ return (path, parse $ scan file)

-- |
-- Given the path to a `Move.toml` file, loads the entire project files
loadToml :: String -> IO [(FilePath, Root)]
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
    loadFolder :: String -> IO [(FilePath, Root)]
    loadFolder folderName = do
      let dirPath = takeDirectory tomlPath </> folderName
      filesInDir <- listDirectory dirPath
      mapM (loadFile . (dirPath </>)) $ filter ((== ".move") . takeExtension) filesInDir