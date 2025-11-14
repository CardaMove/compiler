module Move.Translations.MutationSpec (spec) where

import Move.Lexer (scan)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Move.Parser (parse)
import Test.Hspec
import Move.Translations.Utils (annotateBindingsWithUUID)
import Move.Translations.Mutation (markMutabilityInRoot, MutabilityResult (..))
import Control.Monad.State (evalState)

testMarkMutabilityInRoot :: Spec
testMarkMutabilityInRoot = describe "Tests the function `markMutabilityInRoot`" $ do
  it "Performs the correct mutability analysis" $ do
    moveInput <- readFile "test/Move/Translations/files/MutationSpec_0.move"

    let parsed = parse $ scan moveInput

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    print annotated

    case markMutabilityInRoot annotated of
      MutabilityResult{mutatedVars, assignedToKey, mutatedRefs} -> do
        mutatedVars `shouldBe` Set.fromList [0, 1]
        assignedToKey `shouldBe` Map.fromList [(1, Set.singleton 1), (3, Set.singleton 4)]
        mutatedRefs `shouldBe` Set.singleton 3

spec :: Spec
spec = do
  testMarkMutabilityInRoot