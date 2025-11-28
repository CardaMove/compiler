module Move.Translations.ScopesSpec (spec) where

import Move.Lexer (scan)
import Data.Set qualified as Set
import Move.Parser (parse)
import Test.Hspec
import Move.Translations.Utils (annotateBindingsWithUUID)
import Control.Monad.State (evalState)
import Move.Translations.Scopes (markVariablesForLocalScope, rewriteAssignments, rewriteRefs, rewriteVars, rewriteLetBinds)
import Move.AST
import Move.Translations.TraversalUtils (traverseRootPostOrder, traversalIdentity)

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

    let scopeIdentifier = Identifier "scopes"
    let scopeUUID :: AnnotatedUUID = -1

    rewriteAssignments annotated scopeIdentifier scopeUUID `shouldBe` toModule


testRewriteRefs :: Spec
testRewriteRefs = describe "Tests the function `rewriteRefs`" $ do
  it "Rewrites references and dereferences on right value as getters from the state" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_4.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_5.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    let scopeIdentifier = Identifier "scopes"
    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeIdentifier scopeUUID
    let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
    secondPass `shouldBe` toModule

testRewriteVars :: Spec
testRewriteVars = describe "Tests the function `rewriteVars`" $ do
  it "Rewrites variables on right value as getters from the state, if needed" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_6.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_7.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    let markedVars = markVariablesForLocalScope annotated

    let scopeIdentifier = Identifier "scopes"
    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeIdentifier scopeUUID
    let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
    let thirdPass :: Root = fst $ traverseRootPostOrder (rewriteVars markedVars) traversalIdentity secondPass ()

    thirdPass `shouldBe` toModule


testRewriteLetBinds :: Spec
testRewriteLetBinds = describe "Tests the function `rewriteLetBinds`" $ do
  it "Rewrites let bindings as setters on the state" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_8.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_9.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed) 0

    let markedVars = markVariablesForLocalScope annotated

    let scopeIdentifier = Identifier "scopes"
    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeIdentifier scopeUUID
    let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
    let thirdPass :: Root = fst $ traverseRootPostOrder (rewriteVars markedVars) traversalIdentity secondPass ()
    let fourthPass :: Root = fst $ traverseRootPostOrder traversalIdentity (rewriteLetBinds markedVars scopeIdentifier scopeUUID) thirdPass ()

    fourthPass `shouldBe` toModule


spec :: Spec
spec = do
  testMarkVariablesForLocalScope
  testRewriteAssignments
  testRewriteRefs
  testRewriteVars
  testRewriteLetBinds