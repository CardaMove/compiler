module TypeScript.TypeScriptGeneratorSpec (spec) where

import Data.Text (unpack)
import Move.AST
import Test.Hspec
import TypeScript.TypeScriptGenerator (generateTopLevel)

-- NOTE about identation and line endings
-- Like in the Aiken generator tests, \r and \n characters are removed before comparing

-- | Reads a `show`n TopLevel fixture and compares its generated TypeScript with the expected `.ts` fixture.
-- Top levels are generated as if they belonged to a module (`src/modules/module_<address>/<Module>.ts`)
expectGenerated :: FilePath -> FilePath -> Expectation
expectGenerated inputFile expectedFile = do
  input <- readFile $ "test/TypeScript/files/" ++ inputFile
  let from = read input :: TopLevel
  to <- readFile $ "test/TypeScript/files/" ++ expectedFile
  strip (unpack (generateTopLevel from)) `shouldBe` strip to

testGenerateTopLevel :: Spec
testGenerateTopLevel = describe "Tests the function `generateTopLevel`" $ do
  it "Generates a ConStr0 type and constructor for TopLevelNamedStruct" $
    expectGenerated "TypeScriptGeneratorSpec_0.txt" "TypeScriptGeneratorSpec_1.ts"

  it "Generates a ConStr0 type and constructor for TopLevelPositionalStruct" $
    expectGenerated "TypeScriptGeneratorSpec_2.txt" "TypeScriptGeneratorSpec_3.ts"

  it "Generates the signature and a placeholder body for a function" $
    expectGenerated "TypeScriptGeneratorSpec_4.txt" "TypeScriptGeneratorSpec_5.ts"

  it "Generates namespace and member imports for an aliased use of the utils library" $
    expectGenerated "TypeScriptGeneratorSpec_6.txt" "TypeScriptGeneratorSpec_7.ts"

  it "Generates a namespace import for a use of another module" $
    expectGenerated "TypeScriptGeneratorSpec_8.txt" "TypeScriptGeneratorSpec_9.ts"

  it "Generates a literal constant" $
    expectGenerated "TypeScriptGeneratorSpec_10.txt" "TypeScriptGeneratorSpec_11.ts"

-- |
-- Removes \n and \r characters from a String
-- Used to ignore those characters in test cases
strip :: String -> String
strip = filter (\ch -> ch /= '\r' && ch /= '\n')

spec :: Spec
spec = testGenerateTopLevel
