module Move.Translations.TraversalUtilsSpec (spec) where

import Control.Exception (ErrorCall (ErrorCall), evaluate)
import Control.Monad.State (evalState)
import Data.Map qualified as Map
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TraversalUtils
import Move.Translations.Utils (VariableAnnotations (VariableAnnotations), annotateBindingsWithUUID, getNacFromScopes, numericType)
import Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks)
import Test.Hspec

testTraverseRootPostOrder :: Spec
testTraverseRootPostOrder = describe "Tests the function `traverseRootPostOrder`" $ do
  it "Replaces each literal numeric value with itself + 1" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_1.move"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    -- The traversal function increments by 1 each literal decimal value and keeps track of how many modifications
    -- Also, appends a _2 to each variable name found in let bindings
    -- Note: the output Move code will be invalid due to variable renamings
    let (traversedModule, finalState) = traverseRootPostOrder exprMapper bindsMapper traversalIdentity fromModule 0 []
          where
            exprMapper (ValueLiteral (Numerical (LiteralIntDec val))) _ state = (ValueLiteral $ Numerical $ LiteralIntDec $ val + 1, state + 1)
            exprMapper expr _ state = (expr, state)

            bindsMapper binds@Bindings{bindings} _ state = case bindings of
              BindedSingle (BindIdentifier (Identifier ident) annotations) -> (binds{bindings = BindedSingle $ BindIdentifier (Identifier $ ident ++ "_2") annotations}, state + 1)
              _ -> (binds, state)

    (traversedModule, finalState) `shouldBe` (toModule, 10 :: Integer)

testTimestampStdLibImport :: Spec
testTimestampStdLibImport = describe "Tests the std::timestamp stdlib module" $ do
  it "resolves timestamp::now_seconds to its CPS-threaded signature" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_2.move"

    let module' = parse $ scan moduleStr
        expectedAnnotation =
          VariableAnnotations
            Nothing
            (TypeArrow [] [IntermediateTypeScopes, TypeTuple [numericType, IntermediateTypeScopes]])
        (traversedModule, resolvedAnnotation) = traverseRootPostOrder exprMapper traversalIdentity traversalIdentity module' Nothing []
          where
            exprMapper expr scopes state = case expr of
              PositionalStructExprOrFunctionCallExpr
                PositionalStructExprOrFunctionCall {pseofcNameAccessChain} ->
                  (expr, Just $ getNacFromScopes pseofcNameAccessChain scopes)
              _ -> (expr, state)

    traversedModule `shouldBe` module'
    resolvedAnnotation `shouldBe` Just expectedAnnotation

  it "keeps the timestamp result and CPS state when binding it to a local variable" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_2.move"

    let module' = parse $ scan moduleStr
        expectedSecondsType = TypeTuple [numericType, IntermediateTypeScopes]
        (_, resolvedSecondsType) = traverseRootPostOrder exprMapper traversalIdentity traversalIdentity module' Nothing []
          where
            exprMapper (NameAccessChainExpr (LocalNameAccessChain (Identifier "seconds"))) scopes _ =
              (NameAccessChainExpr (LocalNameAccessChain (Identifier "seconds")), Just $ getNacFromScopes (LocalNameAccessChain (Identifier "seconds")) scopes)
            exprMapper expr _ state = (expr, state)

    resolvedSecondsType `shouldBe` Just (VariableAnnotations Nothing expectedSecondsType)

-- |
-- Tests the traversal of spec blocks (function specs and spec schemas) by `traverseRootPostOrder`:
-- inner expressions of the members, the spec member mapper, and the local scope built from spec variables and spec lets
testTraverseSpecBlocks :: Spec
testTraverseSpecBlocks = describe "Tests the traversal of spec blocks in `traverseRootPostOrder`" $ do
  it "Traverses the inner expressions of the spec members, then maps the members" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"
    toModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_4.move"

    let fromModule = annotateSpecBlocks $ parse $ scan fromModuleStr
    let toModule = annotateSpecBlocks $ parse $ scan toModuleStr

    -- The expression mapper increments by 1 each literal decimal value (also inside spec members),
    -- while the spec member mapper appends a _2 to each spec variable and spec let identifier.
    -- Both keep track of how many modifications have been done. References to renamed identifiers are left untouched
    let (traversedModule, finalState) = traverseRootPostOrder exprMapper traversalIdentity specMemberMapper fromModule 0 []
          where
            exprMapper (ValueLiteral (Numerical (LiteralIntDec val))) _ state = (ValueLiteral $ Numerical $ LiteralIntDec $ val + 1, state + 1)
            exprMapper expr _ state = (expr, state)

            specMemberMapper (SpecBlockMemberLet specLet@SpecLet {specLetIdentifier = Identifier ident}) _ state =
              (SpecBlockMemberLet specLet {specLetIdentifier = Identifier $ ident ++ "_2"}, state + 1)
            specMemberMapper (SpecBlockMemberVariable specVar@SpecVariable {specVariableIdentifier = Identifier ident}) _ state =
              (SpecBlockMemberVariable specVar {specVariableIdentifier = Identifier $ ident ++ "_2"}, state + 1)
            specMemberMapper member _ state = (member, state)

    (traversedModule, finalState) `shouldBe` (toModule, 11 :: Integer)

  it "Passes to the spec member mapper a fresh local scope with the function parameters and the previously declared spec variables and lets" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = annotateSpecBlocks $ parse $ scan moduleStr
        u64Type = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []
        local name annotType = (LocalNameAccessChain $ Identifier name, VariableAnnotations Nothing annotType)
        params = [local "a" u64Type, local "b" u64Type]
        -- The content of the local scope seen by each spec member, in order
        expectedLocalScopes =
          [ -- spec add: starts with the parameters of `add`
            params,
            params ++ [local "one" numericType],
            params ++ [local "one" numericType, local "two" numericType],
            params ++ [local "one" numericType, local "two" numericType],
            -- spec schema AddSchema: the variables of `spec add` are not visible
            [],
            [local "x" u64Type],
            [local "x" u64Type, local "y" (TypeArrow [Identifier "U"] [TypeConstructor (LocalNameAccessChain $ Identifier "U") []])],
            [local "x" u64Type, local "y" (TypeArrow [Identifier "U"] [TypeConstructor (LocalNameAccessChain $ Identifier "U") []]), local "z" numericType],
            [local "x" u64Type, local "y" (TypeArrow [Identifier "U"] [TypeConstructor (LocalNameAccessChain $ Identifier "U") []]), local "z" numericType]
          ]
        (traversedModule, localScopes) = traverseRootPostOrder traversalIdentity traversalIdentity specMemberMapper module' [] []
          where
            specMemberMapper member scopes state = case scopes of
              localScope : _ -> (member, state ++ [Map.toList localScope])
              [] -> error "No scopes given to the spec member mapper"

    traversedModule `shouldBe` module'
    localScopes `shouldBe` expectedLocalScopes

  it "Resolves the spec variables in the expressions of the following spec members" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = annotateSpecBlocks $ parse $ scan moduleStr
        -- Collects the annotations of each reference to `x` and `z`, in order
        (_, resolved) = traverseRootPostOrder exprMapper traversalIdentity traversalIdentity module' [] []
          where
            exprMapper expr@(NameAccessChainExpr nac@(LocalNameAccessChain (Identifier name))) scopes state
              | name `elem` ["x", "z"] = (expr, state ++ [(name, getNacFromScopes nac scopes)])
            exprMapper expr _ state = (expr, state)
        u64Annot = VariableAnnotations Nothing $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") []
        numericAnnot = VariableAnnotations Nothing numericType

    -- `let z = x + 3;`, `include OtherSchema { v: z };`, `ensures z == x + 3;`
    resolved `shouldBe` [("x", u64Annot), ("z", numericAnnot), ("z", numericAnnot), ("x", u64Annot)]

  it "Resolves the parameters of the target function in the expressions of a function spec block" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = annotateSpecBlocks $ parse $ scan moduleStr
        -- Collects the annotations of each reference to `a` and `b`; the ones in the body of `add` come first
        (_, resolved) = traverseRootPostOrder exprMapper traversalIdentity traversalIdentity module' [] []
          where
            exprMapper expr@(NameAccessChainExpr nac@(LocalNameAccessChain (Identifier name))) scopes state
              | name `elem` ["a", "b"] = (expr, state ++ [(name, getNacFromScopes nac scopes)])
            exprMapper expr _ state = (expr, state)
        u64Annot = VariableAnnotations Nothing $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") []

    -- `a + b + 1` in the body, then `ensures result == a + b + one;` in `spec add`
    resolved `shouldBe` [("a", u64Annot), ("b", u64Annot), ("a", u64Annot), ("b", u64Annot)]

  it "Throws an error when a function spec block has no target function annotations (`annotateSpecBlocks` not run)" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = parse $ scan moduleStr
        (traversedModule, ()) = traverseRootPostOrder traversalIdentity traversalIdentity traversalIdentity module' () []

    evaluate (length $ show traversedModule) `shouldThrow` anyErrorCall

testSpecSchemasInScope :: Spec
testSpecSchemasInScope = describe "Tests that spec schemas are added to the scope by `traverseRootPostOrder`" $ do
  it "Adds the spec schemas declared in the module to its scope, with their type parameters and spec variables" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = annotateSpecBlocks $ parse $ scan moduleStr
        -- Collects, for each spec member, the annotations of `AddSchema` found in the scopes
        (_, resolved) = traverseRootPostOrder traversalIdentity traversalIdentity specMemberMapper module' [] []
          where
            specMemberMapper member scopes state = (member, state ++ [getNacFromScopes (LocalNameAccessChain $ Identifier "AddSchema") scopes])

    resolved `shouldBe` replicate 9 addSchemaAnnot

  it "Adds the imported spec schemas to the scope, by module and by (aliased) member" $ do
    importedModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"
    importingModuleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_6.move"

    let importedModule = annotateSpecBlocks $ parse $ scan importedModuleStr
        importingModule = annotateSpecBlocks $ parse $ scan importingModuleStr
        importedAddress = case importedModule of
          RModule Module {moduleAddress} -> moduleAddress
          RScript _ -> error "Expected a module"
        moduleIdent = Identifier "TraversalUtilsSpecBlocks"
        schemaIdent = Identifier "AddSchema"
        nacs =
          [ UnaliasedNameAccessChain importedAddress moduleIdent schemaIdent,
            AliasedNameAccessChain moduleIdent schemaIdent,
            LocalNameAccessChain $ Identifier "Renamed",
            LocalNameAccessChain schemaIdent
          ]
        -- Collects the annotations of each name access chain above, as seen by the only spec member
        (_, resolved) = traverseRootPostOrder traversalIdentity traversalIdentity specMemberMapper importingModule [] [importedModule]
          where
            specMemberMapper member scopes state = (member, state ++ map (`getNacFromScopes` scopes) nacs)

    resolved `shouldBe` replicate 4 addSchemaAnnot

  it "Annotates a spec schema in the scope with the UUID of its spec block" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_3.move"

    let module' = evalState (annotateBindingsWithUUID $ annotateSpecBlocks $ parse $ scan moduleStr) 0
        -- Collects the UUID of `AddSchema` as seen by the first spec member
        (_, resolved) = traverseRootPostOrder traversalIdentity traversalIdentity specMemberMapper module' [] []
          where
            specMemberMapper member scopes state = case getNacFromScopes (LocalNameAccessChain $ Identifier "AddSchema") scopes of
              VariableAnnotations uuid _ -> (member, state ++ [uuid])

    -- `add` and its parameters take UUIDs 0 to 2, then `spec add` 3, the copies of `a` and `b` in its annotations 4 and 5,
    -- `one` 6, `two` 7, and `spec schema AddSchema` 8
    take 1 resolved `shouldBe` [Just 8]

  it "Throws an error when a spec schema has the same name as a struct of the module" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_7.move"

    let module' = parse $ scan moduleStr
        (traversedModule, ()) = traverseRootPostOrder traversalIdentity traversalIdentity traversalIdentity module' () []

    evaluate (length $ show traversedModule) `shouldThrow` errorCall' "spec schema name clash"

  it "Throws an error when a spec schema has the same name as a function of the module" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_8.move"

    let module' = parse $ scan moduleStr
        (traversedModule, ()) = traverseRootPostOrder traversalIdentity traversalIdentity traversalIdentity module' () []

    evaluate (length $ show traversedModule) `shouldThrow` errorCall' "spec schema name clash"

  where
    -- Matches an `ErrorCall` whose message starts with the given prefix
    errorCall' :: String -> Selector ErrorCall
    errorCall' prefix (ErrorCall msg) = take (length prefix) msg == prefix

    -- The expected annotations of `spec schema AddSchema<T> { x: u64; y<U>: U; ... }` in TraversalUtilsSpec_3.move
    addSchemaAnnot =
      VariableAnnotations Nothing $
        IntermediateTypeSpecSchema
          [Identifier "T"]
          [ SpecVariable (Identifier "x") [] (TypeConstructor (LocalNameAccessChain $ Identifier "u64") []) Nothing,
            SpecVariable (Identifier "y") [TypeParameter False (Identifier "U") []] (TypeConstructor (LocalNameAccessChain $ Identifier "U") []) Nothing
          ]

spec :: Spec
spec = do
  testTraverseRootPostOrder
  testTimestampStdLibImport
  testTraverseSpecBlocks
  testSpecSchemasInScope
