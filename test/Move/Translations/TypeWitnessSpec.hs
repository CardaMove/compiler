module Move.Translations.TypeWitnessSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TypeWitness (rewriteTopLevelsInRoot, translateTParamsInRoot)
import Test.Hspec
import Control.Monad.State (evalState)
import Move.Translations.Utils (annotateBindingsWithUUID)

testRewriteTopLevelsInRoot :: Spec
testRewriteTopLevelsInRoot = describe "Tests for the function `rewriteTopLevelsInRoot`" $ do
  it "Appends parameters to function declarations for type witnesses" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_1.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let updated = evalState (annotateBindingsWithUUID parsed >>= rewriteTopLevelsInRoot) 0

    updated `shouldBe` toModule


testTranslateTParamsInRoot :: Spec
testTranslateTParamsInRoot = describe "Tests for the function `translateTParamsInRoot`" $ do
  it "Rewrites all function declarations and invocations to include type witness paras and arguments" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_2.move"
    toModuleStr <- readFile "test/Move/Translations/files/TypeWitnessSpec_3.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let updated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    updated `shouldBe` toModule

spec :: Spec
spec = do
  testRewriteTopLevelsInRoot
  testTranslateTParamsInRoot