module Move.Translations.ScopesSpec (spec) where

import Move.Lexer (scan)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Move.Parser (parse)
import Test.Hspec
import Move.Translations.Utils (annotateBindingsWithUUID)
import Control.Monad.State (evalState)
import Move.Translations.Scopes (markVariablesForLocalScope)

testMarkVariablesForLocalScope :: Spec
testMarkVariablesForLocalScope = describe "Tests the function `markVariablesForLocalScope`" $ do
  it "Returns which variables should be added to the local scope" $ do
    moveInput <- readFile "test/Move/Translations/files/ScopesSpec_0.move"

    let parsed = parse $ scan moveInput

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    print annotated

    markVariablesForLocalScope annotated `shouldBe` Set.fromList [0, 1, 3, 7, 9]


spec :: Spec
spec = do
  testMarkVariablesForLocalScope