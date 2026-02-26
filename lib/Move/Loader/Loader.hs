module Move.Loader.Loader (loadToml, AddrAssociations) where

import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Move.AST (Identifier (Identifier), Numerical (LiteralIntHex), Root)
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Utils (tryIO)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- |
-- Represents addresses associations, from named address' identifier
-- to the corresponding numerical address
type AddrAssociations = Map.Map Identifier Numerical

-- |
-- Loads a single file and parses it
loadFile :: String -> IO (FilePath, Root)
loadFile path = do
  file <- tryIO ("File loading: " ++ show path) $ readFile path
  tryIO ("File parsing: " ++ show path) $ return (path, parse $ scan file)

-- |
-- Given the path to a `Move.toml` file, loads the entire project files
--
-- Also returns the address associations as defined in the `Move.toml` file under `[addresses]` part
loadToml :: String -> IO ([(FilePath, Root)], AddrAssociations)
loadToml tomlPath = do
  tomlExists <- doesFileExist tomlPath
  if tomlExists
    then do
      -- Loads all files inside `./scripts` and `./sources`
      scripts <- loadFolder "scripts"
      sources <- loadFolder "sources"

      -- Load the address associations from the toml
      addrAssoc <- loadAddrAssociations tomlPath

      -- Return parsed files and addess associations
      pure (scripts ++ sources, addrAssoc)
    else error $ "Move.toml file not found at: " ++ show tomlPath
  where
    loadFolder :: String -> IO [(FilePath, Root)]
    loadFolder folderName = do
      let dirPath = takeDirectory tomlPath </> folderName
      filesInDir <- listDirectory dirPath
      mapM (loadFile . (dirPath </>)) $ filter ((== ".move") . takeExtension) filesInDir


-- |
-- Parses the `Move.toml` file to retrieve the address associations
loadAddrAssociations :: FilePath -> IO AddrAssociations
loadAddrAssociations tomlPath = do
  rows <- lines <$> readFile tomlPath

  let addrLines = findAddrRows rows False

  return $ Map.fromList $ map parseAddr addrLines
  where
    -- Given all the lines in the toml, finds the ones correspondng to named addresses
    findAddrRows :: [String] -> Bool -> [String]
    -- End of file
    findAddrRows [] _ = []
    -- "[addresses]" line not yet found, check the current line
    findAddrRows (ln : lns) False = findAddrRows lns $ "[addresses]" `isPrefixOf` ln
    -- "[addresses]" line already found, found end of toml section
    findAddrRows ("" : _) True = []
    findAddrRows ("\n" : _) True = []
    findAddrRows ("\r" : _) True = []
    findAddrRows (('[' : _) : _) True = []
    -- "[addresses]" line already found, all lines until empty row or another toml section are named addresses
    findAddrRows (ln : lns) True = ln : findAddrRows lns True

    -- Helper for `parseAddr`
    parseAddr' :: ((String, String), Bool) -> Char -> ((String, String), Bool)
    -- Ignore whitespaces and equal sign
    parseAddr' acc ' ' = acc
    parseAddr' acc '=' = acc
    -- When finding the first double quote, pass to the numerical section
    parseAddr' ((l, r), True) '\"' = ((l, r), False)
    -- When iterating on the identifier section, collect the characters
    parseAddr' ((l, r), True) ch = ((ch : l, r), True)
    -- When finding the second double quote, just ignore it
    parseAddr' ((l, r), False) '\"' = ((l, r), False)
    -- When iterating on the numerical section, collect the characters
    parseAddr' ((l, r), False) ch = ((l, ch : r), False)

    -- Given an address line from the toml file,
    -- parses it and returns the address identifier and the resolved numerical value
    --
    -- Note: addresses can only specified in hexadecimal values
    --
    -- There is also the possibility of an address with value "_", but it is ignored for now
    parseAddr :: String -> (Identifier, Numerical)
    parseAddr str =
      let (identStr, num) = fst $ foldl parseAddr' (("", ""), True) str
       in (Identifier $ reverse identStr, LiteralIntHex $ reverse num)
