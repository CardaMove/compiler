module Move.Translations.TypeWitnessSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TypeWitness (translateTParamsInRoot, toSnake)
import Test.Hspec
import Control.Monad.State (evalState)
import Move.Translations.Utils (annotateBindingsWithUUID)

testTranslateTParamsInRoot :: Spec
testTranslateTParamsInRoot = describe "Tests for the function `translateTParamsInRoot`" $ do
  it "Rewrites all function declarations and invocations to include type witness paras and arguments" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_2.move"
    toModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_3.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let updated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    updated `shouldBe` toModule

  it "Rewrites named and positional structs declarations so that type args are in snake_case" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_4.move"
    toModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_5.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let updated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    updated `shouldBe` toModule

testToSnake :: Spec
testToSnake = describe "Tests the function `toSnake`" $ do
  it "Should compute the snake_case version of an Identifier, prepended by `tw_` to avoid name clashes" $ do
    toSnake (Identifier "MyString123Hello") `shouldBe` Identifier "tw_my_string123_hello"

spec :: Spec
spec = do
  testTranslateTParamsInRoot
  testToSnake