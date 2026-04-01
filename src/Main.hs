{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.AikenGenerator (generateRoot)
import Aiken.ValidatorGenerator (collectScriptMainMetadata, generateValidator)
import Control.Monad (zipWithM_)
import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Text (unpack)
import Move.AST (Address (NamedAddress, NumericalAddress), Identifier (Identifier), Module (Module, moduleAddress, moduleIdentifier), Numerical (LiteralIntDec, LiteralIntHex), Root (RModule, RScript))
import Move.Loader.Loader (loadToml)
import Move.Transpiler (transpiler)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))


main :: IO ()
main = do
  putStrLn "CardaMove ready"

  -- Load files
  let tomlPath = "test\\Move\\Loader\\files\\Move.toml"

  (sourceFiles, addrAssoc) <- loadToml tomlPath

  let nScripts = length [f | (_, RScript f) <- sourceFiles]
  let nModules = length [f | (_, RModule f) <- sourceFiles]

  putStrLn $ "Loaded " ++ show nScripts ++ " scripts and " ++ show nModules ++ " modules"

  transpiled <- transpiler sourceFiles addrAssoc

  putStrLn "Transpiled"

  -- TODO: force evaluation, throw error with file path that caused it
  aikenText <- mapM (uncurry generateRoot) transpiled

  putStrLn "Generated Aiken text"

  let outDir = "test/out"

  -- For each source file, create the output path of the corresponding Aiken file
  let outFiles = evalState (mapM ((`buildOutFilePath` outDir) . snd) transpiled) 0

  zipWithM_ writeFile' outFiles (map (removeCarriage . unpack) aikenText)

  let validatorPath = outDir </> "validator.ak"
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
buildOutFilePath :: Root -> FilePath -> State Int FilePath
-- Rewrite modules in folder `ModuleName/ModuleIdent.ak`
-- where ModuleName is resolved if a named address
buildOutFilePath (RModule Module {moduleAddress, moduleIdentifier = Identifier identStr}) outDir = do
  let folderName = case moduleAddress of
        NumericalAddress (LiteralIntDec val) -> show val
        NumericalAddress (LiteralIntHex val) -> val
        NamedAddress (Identifier str) -> str
  return $ outDir </> folderName </> identStr <.> ".ak"
-- Scripts are instead written in `scripts/script_idx.ak`
buildOutFilePath (RScript _) outDir = do
  idx <- get
  put $ idx + 1
  return $ outDir </> "scripts" </> "script_" ++ show idx <.> ".ak"

-- |
-- Extension of `writeFile` but ensures all the parent directories exist, then writes the file
writeFile' :: FilePath -> String -> IO ()
writeFile' path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content