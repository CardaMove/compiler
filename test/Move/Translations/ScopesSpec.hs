module Move.Translations.ScopesSpec (spec) where

import Move.Lexer (scan)
import Data.Set qualified as Set
import Move.Parser (parse)
import Test.Hspec
import Move.Translations.Utils (annotateBindingsWithUUID)
import Control.Monad.State (evalState)
import Move.Translations.Scopes (markVariablesForLocalScope, rewriteAssignments)
import Move.AST

testMarkVariablesForLocalScope :: Spec
testMarkVariablesForLocalScope = describe "Tests the function `markVariablesForLocalScope`" $ do
  it "Returns which variables should be added to the local scope" $ do
    moveInput <- readFile "test/Move/Translations/files/ScopesSpec_0.move"

    let parsed = parse $ scan moveInput

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    markVariablesForLocalScope annotated `shouldBe` Set.fromList [1,2,4,8,10]


testRewriteAssignments :: Spec
testRewriteAssignments = describe "Tests the function `rewriteAssignments`" $ do
  it "Rewrites assignments (left side only) as temporary let bindings" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_2.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_3.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    print annotated

    let scopeIdentifier = Identifier "scopes"
    let scopeUUID :: AnnotatedUUID = -1

    -- TODO:
    rewriteAssignments annotated scopeIdentifier scopeUUID `shouldBe` toModule


spec :: Spec
spec = do
  testMarkVariablesForLocalScope
  testRewriteAssignments