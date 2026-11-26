module Move.TranslationsMSL.SpecInliningSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Data.List (isInfixOf)
import Move.AST
import Move.Lexer (scan)
import Move.Loader.Loader (loadToml)
import Move.Parser (parse)
import Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks, stripMoveFunctions)
import Move.TranslationsMSL.SpecInlining (inlineSpecSchemas)
import Test.Hspec

-- |
-- Parses the given fixture files of this module, one root each
parseFixtures :: [Int] -> IO [Root]
parseFixtures = mapM (\n -> parse . scan <$> readFile ("test/Move/TranslationsMSL/files/SpecInliningSpec_" ++ show n ++ ".move"))

-- |
-- Removes the parentheses (single element `CommaExpr`) from the roots, so that a hand-written expected fixture,
-- which needs parentheses to express the shape of the tree, can be compared with the output of the pass
removeParentheses :: [Root] -> [Root]
removeParentheses = transformBi removeParens
  where
    -- Replaces a parenthesised expression with its content
    removeParens :: Expr -> Expr
    removeParens (CommaExpr [e]) = e
    removeParens e = e

-- |
-- Runs `inlineSpecSchemas` on the input fixtures and compares it with the expected fixtures, without parentheses
shouldInlineTo :: [Int] -> [Int] -> Expectation
shouldInlineTo inputs expected = do
  inputRoots <- parseFixtures inputs
  expectedRoots <- parseFixtures expected
  removeParentheses (inlineSpecSchemas inputRoots) `shouldBe` removeParentheses expectedRoots

-- |
-- Runs `inlineSpecSchemas` on the given sources, one root each, and expects an error whose message contains the given text
shouldFailWith :: [String] -> String -> Expectation
shouldFailWith sources message =
  evaluate (length $ show $ inlineSpecSchemas $ map (parse . scan) sources)
    `shouldThrow` (\(ErrorCall msg) -> message `isInfixOf` msg)

-- | A module with two schemas, used by the error tests
errorSchemas :: String
errorSchemas =
  unlines
    [ "module NamedAddr::E {",
      "    spec schema S<T> { x: u64; ensures x > 0; }",
      "    spec schema Loop1 { include Loop2; }",
      "    spec schema Loop2 { include Loop1; }",
      "    spec schema Impure { x: u64; ensures { let y = x; y } > 0; }",
      "}"
    ]

-- | A script whose spec contains the given include member
scriptWithInclude :: String -> String
scriptWithInclude includeMember =
  unlines
    [ "script {",
      "    use NamedAddr::E::{Self, S};",
      "    fun main(n: u64) {}",
      "    spec main { " ++ includeMember ++ " }",
      "}"
    ]

testInlineSpecSchemas :: Spec
testInlineSpecSchemas = describe "Tests the function `inlineSpecSchemas`" $ do
  it "Inlines a schema in its own module and in a script: qualified names, renamed lets, added imports, schemas removed" $ do
    [0, 2] `shouldInlineTo` [1, 3]

  it "Copies the premises of conditional includes into ensures and aborts_if, leaving modifies and lets unchanged" $ do
    [4] `shouldInlineTo` [5]

  it "Inlines nested includes across modules, qualifying each name in the context of its own module" $ do
    [6, 8, 10] `shouldInlineTo` [7, 9, 11]

  it "Leaves spec blocks without includes unchanged" $ do
    roots <- parseFixtures [7, 9]
    inlineSpecSchemas roots `shouldBe` roots

  it "Fails on an unknown schema" $ do
    [errorSchemas, scriptWithInclude "include E::Missing<u64> { x: n };"] `shouldFailWith` "Included spec schema not found"

  it "Fails on an argument naming a variable the schema does not declare" $ do
    [errorSchemas, scriptWithInclude "include S<u64> { y: n };"] `shouldFailWith` "has no variable"

  it "Fails on a wrong number of type arguments, including omitted ones" $ do
    [errorSchemas, scriptWithInclude "include S { x: n };"] `shouldFailWith` "Wrong number of type arguments"

  it "Fails on a cyclic include" $ do
    [errorSchemas, scriptWithInclude "include E::Loop1;"] `shouldFailWith` "Cyclic include"

  it "Fails on a schema with an expression outside the pure fragment" $ do
    [errorSchemas, scriptWithInclude "include E::Impure { x: n };"] `shouldFailWith` "Expression not supported"

  it "Fails on the schema expressions not supported yet" $ do
    [errorSchemas, scriptWithInclude "include S<u64> { x: n } && S<u64> { x: n };"] `shouldFailWith` "not supported yet"
    [errorSchemas, scriptWithInclude "include if (n > 0) S<u64> { x: n } else S<u64> { x: n };"] `shouldFailWith` "not supported yet"

  it "Fails on an expression that is not a schema expression" $ do
    [errorSchemas, scriptWithInclude "include 1;"] `shouldFailWith` "Not a schema expression"

  it "Leaves no include and no schema in the Bet project" $ do
    (sourceFiles, _) <- loadToml "test/Bet/Move.toml"
    let inlined = inlineSpecSchemas $ stripMoveFunctions $ map (annotateSpecBlocks . snd) sourceFiles
    [() | SpecBlockMemberInclude _ <- universeBi inlined] `shouldBe` []
    [() | SpecBlockTargetSchema _ _ <- universeBi inlined] `shouldBe` []

spec :: Spec
spec = testInlineSpecSchemas
