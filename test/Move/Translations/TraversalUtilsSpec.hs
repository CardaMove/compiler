module Move.Translations.TraversalUtilsSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TraversalUtils
import Test.Hspec

testTraverseRootPostOrder :: Spec
testTraverseRootPostOrder = describe "Tests the function `traverseRootPostOrder`" $ do
  it "Replaces each literal numeric value with itself + 1" $ do
    let fromModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " const MY_CONST: u64 = 0;\n"
            ++
            -- This is not valid move, but here is just for testing
            " const MY_CONST2: u64 = {MY_CONST + 6};\n"
            ++ " public fun test(x: u64) {\n"
            ++ "   let a = 12;\n"
            ++ "   let b = 6 * 7;\n"
            ++ "   let c = {a + b + MY_CONST + 1};\n"
            ++ "   c + 10\n"
            ++ " }\n"
            ++ "}"

    let toModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " const MY_CONST: u64 = 1;\n"
            ++ " const MY_CONST2: u64 = {MY_CONST + 7};\n"
            ++ " public fun test(x: u64) {\n"
            ++ "   let a = 13;\n"
            ++ "   let b = 7 * 8;\n"
            ++ "   let c = {a + b + MY_CONST + 2};\n"
            ++ "   c + 11\n"
            ++ " }\n"
            ++ "}"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    -- The traversal function increments by 1 each literal decimal value and keeps track of how many modifications
    let (traversedModule, finalState) = traverseRootPostOrder f fromModule 0
          where
            f (ValueLiteral (Numerical (LiteralIntDec val))) _ state = (ValueLiteral $ Numerical $ LiteralIntDec $ val + 1, state + 1)
            f expr _ state = (expr, state)

    (traversedModule, finalState) `shouldBe` (toModule, 7 :: Integer)

spec :: Spec
spec = do
  testTraverseRootPostOrder