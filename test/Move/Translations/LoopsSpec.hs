module Move.Translations.LoopsSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Loops (translateLoopsToWhile, translateWhilesToFunctionsInRoot)
import Test.Hspec

testLoopsToWhile :: Spec
testLoopsToWhile = describe "Translating a loop into a while" $ do
  it "Translates a Loop Expr" $ do
    let loopBody =
          SequenceExpr $
            Sequence
              { sequenceUses = [],
                sequenceItems =
                  [ -- let a = 10
                    SequenceItemBindExpr $
                      Bindings
                        { bindings = BindedSingle $ BindIdentifier (Identifier "a") Nothing,
                          bindingsBindType = Nothing,
                          bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10
                        }
                  ],
                -- return a
                sequenceEndExpr = Just $ Return $ Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a"
              }

    translateLoopsToWhile (Loop loopBody)
      `shouldBe` ( WhileTerm $
                     While
                       { whileCondition = ValueLiteral $ Boolean True,
                         whileExpr = loopBody
                       }
                 )

  it "Translates nested Loop expressions" $ do
    fromFunctionBodyStr <- readFile "test/Move/Translations/files/LoopsSpec_4.txt"
    toFunctionBodyStr <- readFile "test/Move/Translations/files/LoopsSpec_5.txt"

    let fromFunctionBody = read fromFunctionBodyStr :: Sequence
    let toFunctionBody = read toFunctionBodyStr :: Sequence


    translateLoopsToWhile fromFunctionBody
      `shouldBe` toFunctionBody

testTranslateWhilesToFunctionsInRoot :: Spec
testTranslateWhilesToFunctionsInRoot = describe "Tests for the function `translateWhilesToFunctionsInRoot`" $ do
  it "Translates a while loop into a function declaration" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/LoopsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/LoopsSpec_1.move"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    translateWhilesToFunctionsInRoot fromModule `shouldBe` toModule

  it "Translates a while loop with break and continue inside" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/LoopsSpec_2.move"
    toModuleStr <- readFile "test/Move/Translations/files/LoopsSpec_3.move"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    translateWhilesToFunctionsInRoot fromModule `shouldBe` toModule

spec :: Spec
spec = do
  testLoopsToWhile
  testTranslateWhilesToFunctionsInRoot