module Move.Translations.TraversalUtilsSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TraversalUtils
import Test.Hspec

testTraverseRootPostOrder :: Spec
testTraverseRootPostOrder = describe "Tests the function `traverseRootPostOrder`" $ do
  it "Replaces each literal numeric value with itself + 1" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_1.move"

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