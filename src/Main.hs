{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.AikenGenerator (generateRoot)
import Control.Monad (zipWithM_)
import Data.Text (unpack)
import Move.AST (Root (RModule, RScript))
import Move.Loader.Loader (loadToml)
import Move.Transpiler (transpiler)
import System.FilePath (replaceExtension)

main :: IO ()
main = do
  putStrLn "CardaMove ready"

  -- Load files
  let tomlPath = "test/Move/Loader/files/Move.toml"

  files <- loadToml tomlPath

  let nScripts = length [f | (_, RScript f) <- files]
  let nModules = length [f | (_, RModule f) <- files]

  putStrLn $ "Loaded " ++ show nScripts ++ " scripts and " ++ show nModules ++ " modules"

  transpiled <- transpiler $ map snd files

  putStrLn "Transpiled"

  let aikenText = map generateRoot transpiled

  putStrLn "Generated Aiken text"

  let outDir = "test/out"

  let outFiles = map (replaceExtension "ak" . fst) files

  zipWithM_ writeFile outFiles (map unpack aikenText)

  putStrLn $ "Written files at " ++ outDir