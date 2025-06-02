{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aiken.UnLexer
import Aiken.UnParser
import Move.Lexer
import Move.Parser
import Move.AST
import Aiken.AST
import Options.Applicative
import Translator (translate)

data Sample = Sample
  { hello :: String,
    quiet :: Bool,
    enthusiasm :: Int
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "hello"
          <> metavar "TARGET"
          <> help "Target for the greeting"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
      )
    <*> option
      auto
      ( long "enthusiasm"
          <> help "How enthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

main2 :: IO ()
main2 = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()

main :: IO ()
main = do
  let mod = "module foo::bar { struct Baz has key { a: bool, b: u8 } }"
  let mov = parse $ scan mod
  let aik :: Aiken.AST.Module = translate (mov :: Move.AST.Module)
  let src = unLex $ unParse aik
  print "Move source code:"
  print mod
  putStrLn ""
  putStrLn "Move AST:"
  print mov
  putStrLn ""
  putStrLn "Aiken AST:"
  print aik
  putStrLn ""
  putStrLn "Aiken source code:"
  print src
