module Aiken.CodeGeneratorSpec (spec) where

import Aiken.CodeGenerator (generateTopLevel)
import Data.Text (unpack)
import Move.AST
import Test.Hspec

-- NOTE about identation and line endings
-- Aiken identation size defaults to two spaces
-- The generator creates \r before each newline, which are removed (along with newlines for easing tests) on every test case

testGenerateTopLevel :: Spec
testGenerateTopLevel = describe "Tests the function `generateTopLevel`" $ do
  it "Generates Text for TopLevelNamedStruct" $ do
    input <- readFile "test/Aiken/files/CodeGeneratorSpec_0.txt"
    let from = read input :: TopLevel

    to <- readFile "test/Aiken/files/CodeGeneratorSpec_1.ak"

    strip (unpack (generateTopLevel from)) `shouldBe` strip to

  it "Generates Text for TopLevelPositionalStruct" $ do
    input <- readFile "test/Aiken/files/CodeGeneratorSpec_2.txt"
    let from = read input :: TopLevel

    to <- readFile "test/Aiken/files/CodeGeneratorSpec_3.ak"

    strip (unpack (generateTopLevel from)) `shouldBe` strip to
  
  -- TODO: Add test for top level constants after expressions are translated

-- |
-- Removes \n and \r characters from a String
-- Used to ignore those characters in test cases
strip :: String -> String
strip = filter (\ch -> ch /= '\r' && ch /= '\n')

spec :: Spec
spec = do
  testGenerateTopLevel