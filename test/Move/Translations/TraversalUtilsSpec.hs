module Move.Translations.TraversalUtilsSpec (spec) where

import Control.Exception (evaluate)
import Data.Map qualified as Map
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.TraversalUtils
import Move.Translations.Utils (VariableAnnotations (VariableAnnotations), getNacFromScopes, numericType)
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

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

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

    let module' = parse $ scan moduleStr
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

    let module' = parse $ scan moduleStr
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

    let module' = parse $ scan moduleStr
        -- Collects the annotations of each reference to `a` and `b`; the ones in the body of `add` come first
        (_, resolved) = traverseRootPostOrder exprMapper traversalIdentity traversalIdentity module' [] []
          where
            exprMapper expr@(NameAccessChainExpr nac@(LocalNameAccessChain (Identifier name))) scopes state
              | name `elem` ["a", "b"] = (expr, state ++ [(name, getNacFromScopes nac scopes)])
            exprMapper expr _ state = (expr, state)
        u64Annot = VariableAnnotations Nothing $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") []

    -- `a + b + 1` in the body, then `ensures result == a + b + one;` in `spec add`
    resolved `shouldBe` [("a", u64Annot), ("b", u64Annot), ("a", u64Annot), ("b", u64Annot)]

  it "Throws an error when the target function of a spec block is not declared in the module" $ do
    moduleStr <- readFile "test/Move/Translations/files/TraversalUtilsSpec_5.move"

    let module' = parse $ scan moduleStr
        (traversedModule, ()) = traverseRootPostOrder traversalIdentity traversalIdentity traversalIdentity module' () []

    evaluate (length $ show traversedModule) `shouldThrow` anyErrorCall

spec :: Spec
spec = do
  testTraverseRootPostOrder
  testTimestampStdLibImport
  testTraverseSpecBlocks