module Move.Translations.TraversalUtilsSpec (spec) where

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
    let (traversedModule, finalState) = traverseRootPostOrder exprMapper bindsMapper fromModule 0 []
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
        (traversedModule, resolvedAnnotation) = traverseRootPostOrder exprMapper traversalIdentity module' Nothing []
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
        (_, resolvedSecondsType) = traverseRootPostOrder exprMapper traversalIdentity module' Nothing []
          where
            exprMapper (NameAccessChainExpr (LocalNameAccessChain (Identifier "seconds"))) scopes _ =
              (NameAccessChainExpr (LocalNameAccessChain (Identifier "seconds")), Just $ getNacFromScopes (LocalNameAccessChain (Identifier "seconds")) scopes)
            exprMapper expr _ state = (expr, state)

    resolvedSecondsType `shouldBe` Just (VariableAnnotations Nothing expectedSecondsType)

spec :: Spec
spec = do
  testTraverseRootPostOrder
  testTimestampStdLibImport