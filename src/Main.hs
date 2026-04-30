{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.AikenGenerator (generateRoot)
import Aiken.ValidatorGenerator (ScriptMainMetadata (..), collectScriptMainMetadata, generateValidator)
import Control.Monad (when, zipWithM_)
import Control.Monad.State (State, evalState)
import Data.List (find)
import Data.Text (unpack)
import Move.AST (Address (NamedAddress, NumericalAddress), Identifier (Identifier), Module (Module, moduleAddress, moduleIdentifier), Numerical (LiteralIntDec, LiteralIntHex), Root (RModule, RScript))
import Move.Loader.Loader (loadToml)
import Move.Transpiler (transpiler)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory, removePathForcibly)
import System.FilePath (takeBaseName, takeDirectory, (<.>), (</>))

main :: IO ()
main = do
  putStrLn "CardaMove ready"

  -- Load files
  let tomlPath = "test\\Bet\\Move.toml" -- "test\\Move\\Loader\\files\\Move.toml"
  (sourceFiles, addrAssoc) <- loadToml tomlPath

  let nScripts = length [f | (_, RScript f) <- sourceFiles]
  let nModules = length [f | (_, RModule f) <- sourceFiles]

  putStrLn $ "Loaded " ++ show nScripts ++ " scripts and " ++ show nModules ++ " modules"

  transpiled <- transpiler sourceFiles addrAssoc

  putStrLn "Transpiled"

  -- TODO: force evaluation, throw error with file path that caused it
  aikenText <- mapM (uncurry generateRoot) transpiled

  putStrLn "Generated Aiken text"

  let outDir = "aiken_out"
  let templateDir = "resources/template_project"

  cleanOutDir outDir
  copyDirectoryContents templateDir outDir

  -- For each source file, create the output path of the corresponding Aiken file
  let scriptsMeta = collectScriptMainMetadata transpiled
  let outFiles = evalState (mapM (\(fp, root) -> buildOutFilePath fp root (outDir </> "lib") scriptsMeta) transpiled) 0

  zipWithM_ writeFile' outFiles (map (removeCarriage . unpack) aikenText)

  -- The Aiken compiler forbids the name "validator" as the name for the validator itself
  -- therefore, it is named as main.ak
  let validatorPath = outDir </> "validators" </> "main.ak"
  let validatorText = generateValidator $ collectScriptMainMetadata transpiled

  writeFile' validatorPath (removeCarriage . unpack $ validatorText)

  putStrLn $ "Written files at " ++ outDir

-- |
-- Used to remove `\r` characters inserted by the Aiken generator
removeCarriage :: String -> String
removeCarriage = filter (/= '\r')

-- |
-- Given module and the out directory,
-- builds the output path for the corresponding Aiken file
buildOutFilePath :: FilePath -> Root -> FilePath -> [ScriptMainMetadata] -> State Int FilePath
-- Rewrite modules in folder `ModuleName/ModuleIdent.ak`
-- where ModuleName is resolved if a named address
buildOutFilePath _filePath (RModule Module {moduleAddress, moduleIdentifier = Identifier identStr}) outDir _scriptsMeta = do
  let folderName =
        "module_" ++ case moduleAddress of
          NumericalAddress (LiteralIntDec val) -> show val
          NumericalAddress (LiteralIntHex val) -> val
          NamedAddress (Identifier str) -> str
  return $ outDir </> folderName </> identStr <.> ".ak"
-- Scripts are written using the original Move script's base name
buildOutFilePath filePath (RScript _) outDir scriptsMeta = do
  -- Prefer the `scriptImportPath` from the collected metadata when available.
  let maybeMeta = find (\m -> takeBaseName (scriptImportPath m) == takeBaseName filePath) scriptsMeta
  let base = case maybeMeta of
        Just m -> takeBaseName (scriptImportPath m)
        Nothing -> takeBaseName filePath
  return $ outDir </> "scripts" </> base <.> ".ak"

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
-- Copies all files and folders from a template directory into the output directory
-- No intermediate root folder is added
copyDirectoryContents :: FilePath -> FilePath -> IO ()
copyDirectoryContents srcDir dstDir = do
  contents <- listDirectory srcDir
  mapM_ (copyEntry srcDir dstDir) contents
  where
    copyEntry :: FilePath -> FilePath -> FilePath -> IO ()
    copyEntry src dst name = do
      let srcPath = src </> name
      let dstPath = dst </> name
      isDir <- doesDirectoryExist srcPath
      if isDir
        then do
          createDirectoryIfMissing True dstPath
          copyDirectoryContents srcPath dstPath
        else copyFile srcPath dstPath