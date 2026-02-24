module Move.Translations.ScopesSpec (spec) where

import Move.Lexer (scan)
import Data.Set qualified as Set
import Move.Parser (parse)
import Test.Hspec
import Move.Translations.Utils (annotateBindingsWithUUID)
import Control.Monad.State (evalState, runState)
import Move.Translations.Scopes (markVariablesForLocalScope, rewriteAssignments, rewriteRefs, rewriteVars, rewriteLetBinds, addLocalScopeInRoot)
import Move.AST
import Move.Translations.TraversalUtils (traverseRootPostOrder, traversalIdentity)
import Move.Translations.TypeWitness (translateTParamsInRoot)

testMarkVariablesForLocalScope :: Spec
testMarkVariablesForLocalScope = describe "Tests the function `markVariablesForLocalScope`" $ do
  it "Returns which variables should be added to the local scope" $ do
    moveInput <- readFile "test/Move/Translations/files/ScopesSpec_0.move"

    let parsed = parse $ scan moveInput

    let annotated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    markVariablesForLocalScope annotated `shouldBe` Set.fromList [1,2,4,8,10]


testRewriteAssignments :: Spec
testRewriteAssignments = describe "Tests the function `rewriteAssignments`" $ do
  it "Rewrites assignments (left side only) as temporary let bindings" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_2.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_3.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let scopeUUID :: AnnotatedUUID = -1

    rewriteAssignments annotated scopeUUID `shouldBe` toModule


testRewriteRefs :: Spec
testRewriteRefs = describe "Tests the function `rewriteRefs`" $ do
  it "Rewrites references and dereferences on right value as getters from the state" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_4.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_5.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeUUID
    let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
    secondPass `shouldBe` toModule

testRewriteVars :: Spec
testRewriteVars = describe "Tests the function `rewriteVars`" $ do
  it "Rewrites variables on right value as getters from the state, if needed" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_6.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_7.txt"

    let parsed = parse $ scan fromModuleStr

    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeUUID
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

    let annotated = evalState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let scopeUUID :: AnnotatedUUID = -1

    let firstPass :: Root = rewriteAssignments annotated scopeUUID
    let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
    let thirdPass :: Root = fst $ traverseRootPostOrder (rewriteVars markedVars) traversalIdentity secondPass ()
    let fourthPass :: Root = fst $ traverseRootPostOrder traversalIdentity (rewriteLetBinds markedVars scopeUUID) thirdPass ()

    fourthPass `shouldBe` toModule

testAddLocalScopeInRoot :: Spec
testAddLocalScopeInRoot = describe "Tests the function `addLocalScopeInRoot`" $ do
  it "Handles function params, scopes in CPS, assignments, references, dot accesses" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_1.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_10.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root


    let (annotated, currUUID) = runState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let updated = addLocalScopeInRoot annotated markedVars currUUID

    updated `shouldBe` toModule

  it "Handles nested sequences, function calls" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_11.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_12.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root


    let (annotated, currUUID) = runState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let updated = addLocalScopeInRoot annotated markedVars currUUID

    updated `shouldBe` toModule

  it "Handles global storage operations and resolves type parameters, handles syntactic sugar on references" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_13.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_14.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root


    let (annotated, currUUID) = runState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let updated = addLocalScopeInRoot annotated markedVars currUUID

    updated `shouldBe` toModule

  it "Rewrites if-then-else accordingly if the branches mutate the state, respecting the resulting type" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_15.move"
    toModuleStr <- readFile "test/Move/Translations/files/ScopesSpec_16.txt"

    let parsed = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root


    let (annotated, currUUID) = runState (annotateBindingsWithUUID parsed >>= translateTParamsInRoot) 0

    let markedVars = markVariablesForLocalScope annotated

    let updated = addLocalScopeInRoot annotated markedVars currUUID

    -- FIXME: Was expecting that a sequence branch would be translated into a single temporary variable
    -- but it is not. Still working
    updated `shouldBe` toModule


spec :: Spec
spec = do
  testMarkVariablesForLocalScope
  testRewriteAssignments
  testRewriteRefs
  testRewriteVars
  testRewriteLetBinds
  testAddLocalScopeInRoot