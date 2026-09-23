module Move.Translations.AlphaConversionSpec (spec) where

import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.AlphaConversion (alphaConvert)
import Test.Hspec

-- | Checks the direct same-scope collision fixture.
testAlphaConvert :: Spec
testAlphaConvert = describe "Tests the function `alphaConvert`" $ do
  it "renames a colliding binding and its later references" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion_0.move"
    expected <- readFile "test/Move/Translations/files/AlphaConversion_0.txt"
    alphaConvert [parse (scan source)] `shouldBe` [read expected]
  it "keeps shadowing in nested scopes independent" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion_1.move"
    expected <- readFile "test/Move/Translations/files/AlphaConversion_1.txt"
    alphaConvert [parse (scan source)] `shouldBe` [read expected]
  it "renames duplicate identifiers in tuple bindings" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion_2.move"
    expected <- readFile "test/Move/Translations/files/AlphaConversion_2.txt"
    alphaConvert [parse (scan source)] `shouldBe` [read expected]
  it "renames identifiers in named-struct binding fields without changing labels" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion_3.move"
    expected <- readFile "test/Move/Translations/files/AlphaConversion_3.txt"
    alphaConvert [parse (scan source)] `shouldBe` [read expected]
  it "renames a variable redeclared by named-struct destructuring" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion_4.move"
    expected <- readFile "test/Move/Translations/files/AlphaConversion_4.txt"
    alphaConvert [parse (scan source)] `shouldBe` [read expected]

spec :: Spec
spec = testAlphaConvert
