module Move.Translations.PolymorphismSpec (spec) where

import Control.Monad.State (runState, evalState)
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Polymorphism (translatePolymorphismInRoot)
import Move.Translations.Scopes (addLocalScopeInRoot, markVariablesForLocalScope)
import Move.Translations.TypeWitness (translateTParamsInRoot)
import Move.Translations.Utils (annotateBindingsWithUUID)
import Test.Hspec

testTranslatePolymorphismInRoot :: Spec
testTranslatePolymorphismInRoot = describe "Tests the function `translatePolymorphismInRoot`" $ do
  it "Correctly performs casts both for function arguments and return. Also translated type parameters occurrences with Data" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/PolymorphismSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/PolymorphismSpec_1.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let (annotated, currUUID) = runState (mapM annotateBindingsWithUUID [parsed] >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let updated = evalState (addLocalScopeInRoot annotated markedVars >>= translatePolymorphismInRoot) currUUID
    
    updated `shouldBe` [toModule]

spec :: Spec
spec = do
  testTranslatePolymorphismInRoot
