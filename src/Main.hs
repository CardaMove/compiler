{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.AikenGenerator qualified as Aiken
import Aiken.ValidatorGenerator (ScriptMainMetadata (..), collectScriptMainMetadata, generateValidator)
import Control.Applicative ((<|>))
import Control.Monad (when, zipWithM_)
import Control.Monad.State (State, evalState)
import Data.List (find)
import Data.Text (unpack)
import Move.AST (Address (NamedAddress, NumericalAddress), Identifier (Identifier), Module (Module, moduleAddress, moduleIdentifier), Numerical (LiteralIntDec, LiteralIntHex), Root (RModule, RScript))
import Move.Loader.Loader (loadToml)
import Move.Transpiler (offchainTranspiler, transpiler)
import Options.Applicative (Parser, ParserInfo, argument, execParser, flag', fullDesc, help, helper, info, long, metavar, progDesc, str)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory, removePathForcibly)
import System.FilePath (normalise, takeBaseName, takeDirectory, (<.>), (</>))
import TypeScript.EntryPointGenerator (generateEntryPoints)
import TypeScript.TypeScriptGenerator qualified as TypeScript

data CommandLineOptions = CommandLineOptions
  { tomlPathOption :: FilePath,
    targetOption :: Target
  }

-- | Which code the compiler generates: on-chain (Aiken), off-chain (TypeScript) or both
data Target = BothTargets | OnchainOnly | OffchainOnly
  deriving (Eq)

-- | Parses the required path to the Move.toml file and the optional target selection.
-- The path is normalized so Windows separators and redundant separators work.
commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser =
  CommandLineOptions . normalise
    <$> argument
      str
      ( metavar "TOML_PATH"
          <> help "Path to the Move.toml file of the Move project to compile"
      )
    <*> targetParser

-- | Parses the mutually exclusive `--onchain-only` / `--offchain-only` switches.
-- Without either of them, both the Aiken and the TypeScript projects are generated.
targetParser :: Parser Target
targetParser =
  flag' OnchainOnly (long "onchain-only" <> help "Only generate the on-chain Aiken project (aiken_out/)")
    <|> flag' OffchainOnly (long "offchain-only" <> help "Only generate the off-chain TypeScript project (ts_out/)")
    <|> pure BothTargets

-- | Builds the CLI parser metadata for compiler usage and help output.
-- The helper parser provides the standard -h and --help options.
commandLineParserInfo :: ParserInfo CommandLineOptions
commandLineParserInfo =
  info
    (helper <*> commandLineOptionsParser)
    ( fullDesc
        <> progDesc "Compile a Move project into Aiken (on-chain, aiken_out/) and TypeScript + Mesh (off-chain, ts_out/)"
    )

main :: IO ()
main = do
  options <- execParser commandLineParserInfo
  putStrLn "CardaMove ready"

  -- Load files
  let tomlPath = tomlPathOption options
  (sourceFiles, addrAssoc) <- loadToml tomlPath

  let nScripts = length [f | (_, RScript f) <- sourceFiles]
  let nModules = length [f | (_, RModule f) <- sourceFiles]

  putStrLn $ "Loaded " ++ show nScripts ++ " scripts and " ++ show nModules ++ " modules"

  transpiled <- transpiler sourceFiles addrAssoc

  putStrLn "Transpiled"

  let target = targetOption options
  when (target /= OffchainOnly) $ writeOnchain transpiled
  when (target /= OnchainOnly) $ do
    offchain <- offchainTranspiler transpiled
    putStrLn "Applied off-chain passes"
    writeOffchain offchain

-- |
-- Generates the Aiken project in `aiken_out/`: the template, one file per module/script and the validator.
-- The roots are the output of the shared `transpiler`
writeOnchain :: [(FilePath, Root)] -> IO ()
writeOnchain transpiled = do
  -- TODO: force evaluation, throw error with file path that caused it
  aikenText <- mapM (uncurry Aiken.generateRoot) transpiled

  putStrLn "Generated Aiken text"

  let outDir = "aiken_out"
  let templateDir = "resources/aiken_template"

  cleanOutDir outDir
  copyDirectoryContents [] templateDir outDir

  -- For each source file, create the output path of the corresponding Aiken file
  let scriptsMeta = collectScriptMainMetadata transpiled
  let outFiles = evalState (mapM (\(fp, root) -> buildOutFilePath fp root (outDir </> "lib") (outDir </> "lib" </> "scripts") ".ak" scriptsMeta) transpiled) 0

  zipWithM_ writeFile' outFiles (map (removeCarriage . unpack) aikenText)

  -- The Aiken compiler forbids the name "validator" as the name for the validator itself
  -- therefore, it is named as main.ak
  let validatorPath = outDir </> "validators" </> "main.ak"
  let validatorText = generateValidator scriptsMeta

  writeFile' validatorPath (removeCarriage . unpack $ validatorText)

  putStrLn $ "Written files at " ++ outDir

-- |
-- Generates the TypeScript project in `ts_out/`: the template (without npm artifacts),
-- one file per module/script and the entry points. The roots are the output of `offchainTranspiler`
writeOffchain :: [(FilePath, Root)] -> IO ()
writeOffchain transpiled = do
  tsText <- mapM (uncurry TypeScript.generateRoot) transpiled

  putStrLn "Generated TypeScript text"

  let outDir = "ts_out"
  let templateDir = "resources/typescript_template"

  cleanOutDir outDir
  -- Artifacts of a local `npm install` / build of the template must not end up in the output
  copyDirectoryContents ["node_modules", "bin", ".env", "wallets"] templateDir outDir

  let scriptsMeta = collectScriptMainMetadata transpiled
  let srcDir = outDir </> "src"
  let outFiles = evalState (mapM (\(fp, root) -> buildOutFilePath fp root (srcDir </> "modules") (srcDir </> "scripts") ".ts" scriptsMeta) transpiled) 0

  zipWithM_ writeFile' outFiles (map (removeCarriage . unpack) tsText)

  writeFile' (srcDir </> "entrypoints.ts") (removeCarriage . unpack $ generateEntryPoints scriptsMeta)

  putStrLn $ "Written files at " ++ outDir

-- |
-- Used to remove `\r` characters inserted by the code generators
removeCarriage :: String -> String
removeCarriage = filter (/= '\r')

-- |
-- Given a source file and the output folders for modules and scripts,
-- builds the output path of the corresponding generated file, with the given extension
buildOutFilePath :: FilePath -> Root -> FilePath -> FilePath -> String -> [ScriptMainMetadata] -> State Int FilePath
-- Rewrite modules in folder `module_<address>/ModuleIdent.<ext>`
-- where the address is resolved if a named address
buildOutFilePath _filePath (RModule Module {moduleAddress, moduleIdentifier = Identifier identStr}) modulesDir _scriptsDir ext _scriptsMeta = do
  let folderName =
        "module_" ++ case moduleAddress of
          NumericalAddress (LiteralIntDec val) -> show val
          NumericalAddress (LiteralIntHex val) -> val
          NamedAddress (Identifier addressName) -> addressName
  return $ modulesDir </> folderName </> identStr <.> ext
-- Scripts are written using the original Move script's base name
buildOutFilePath filePath (RScript _) _modulesDir scriptsDir ext scriptsMeta = do
  -- Prefer the `scriptImportPath` from the collected metadata when available.
  let maybeMeta = find (\m -> takeBaseName (scriptImportPath m) == takeBaseName filePath) scriptsMeta
  let base = case maybeMeta of
        Just m -> takeBaseName (scriptImportPath m)
        Nothing -> takeBaseName filePath
  return $ scriptsDir </> base <.> ext

-- |
-- Extension of `writeFile` but ensures all the parent directories exist, then writes the file
writeFile' :: FilePath -> String -> IO ()
writeFile' path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

-- |
-- Removes the output directory if it exists, then recreates it
cleanOutDir :: FilePath -> IO ()
cleanOutDir outDir = do
  exists <- doesPathExist outDir
  when exists $ removePathForcibly outDir
  createDirectoryIfMissing True outDir

-- |
-- Copies all files and folders from a template directory into the output directory,
-- skipping any entry (at any depth) whose name is in the exclusion list
-- No intermediate root folder is added
copyDirectoryContents :: [FilePath] -> FilePath -> FilePath -> IO ()
copyDirectoryContents excluded srcDir dstDir = do
  contents <- listDirectory srcDir
  mapM_ (copyEntry srcDir dstDir) (filter (`notElem` excluded) contents)
  where
    copyEntry :: FilePath -> FilePath -> FilePath -> IO ()
    copyEntry src dst name = do
      let srcPath = src </> name
      let dstPath = dst </> name
      isDir <- doesDirectoryExist srcPath
      if isDir
        then do
          createDirectoryIfMissing True dstPath
          copyDirectoryContents excluded srcPath dstPath
        else copyFile srcPath dstPath
