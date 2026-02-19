{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.AikenGenerator (generateRoot)
import Control.Monad (zipWithM_)
import Data.Text (unpack)
import Move.AST (Root (RModule, RScript))
import Move.Loader.Loader (loadToml)
import Move.Transpiler (transpiler)
import System.FilePath (replaceExtension, takeFileName, (</>))

main :: IO ()
main = do
  putStrLn "CardaMove ready"

  -- Load files
  let tomlPath = "test/Move/Loader/files/Move.toml"

  files <- loadToml tomlPath

  let nScripts = length [f | (_, RScript f) <- files]
  let nModules = length [f | (_, RModule f) <- files]

  putStrLn $ "Loaded " ++ show nScripts ++ " scripts and " ++ show nModules ++ " modules"

  transpiled <- transpiler files

  putStrLn "Transpiled"

  -- TODO: force evaluation, throw error with file path that caused it
  aikenText <- mapM (uncurry generateRoot) transpiled

  putStrLn "Generated Aiken text"

  let outDir = "test/out"

  -- -- TODO: NOTE: outDir must already exist

  let outFiles = map ((((outDir </>) . (`replaceExtension` "ak")) . takeFileName) . fst) files

  zipWithM_ writeFile outFiles (map (removeCarriage . unpack) aikenText)

  putStrLn $ "Written files at " ++ outDir

-- |
-- Used to remove `\r` characters inserted by the Aiken generator
removeCarriage :: String -> String
removeCarriage = filter (/= '\r')