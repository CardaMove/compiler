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
                        { bindings = BindedSingle $ BindIdentifier $ Identifier "a",
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
    let innerLoopBody =
          SequenceExpr $
            Sequence
              { sequenceUses = [],
                sequenceItems = [],
                -- a = a + 1
                sequenceEndExpr =
                  Just $
                    AssignmentExpr $
                      Assignment
                        { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a",
                          assignmentRight =
                            BinaryOpExprExpr $
                              Add
                                (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a")
                                (ValueLiteral $ Numerical $ LiteralIntDec 1)
                        }
              }

    let functionBody =
          Sequence
            { sequenceUses = [],
              sequenceItems = [],
              -- outer loop
              sequenceEndExpr =
                Just $
                  Loop $
                    SequenceExpr $
                      Sequence
                        { sequenceUses = [],
                          sequenceItems =
                            [ -- let a = 10
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier $ Identifier "a",
                                    bindingsBindType = Nothing,
                                    bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10
                                  },
                              -- inner loop
                              SequenceItemExpr $ Loop $ innerLoopBody
                            ],
                          -- return a
                          sequenceEndExpr = Just $ Return $ Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a"
                        }
            }

    translateLoopsToWhile functionBody
      `shouldBe` ( Sequence
                     { sequenceUses = [],
                       sequenceItems = [],
                       -- outer loop remapped in while
                       sequenceEndExpr =
                         Just $
                           WhileTerm $
                             While
                               { whileCondition = ValueLiteral $ Boolean True,
                                 whileExpr =
                                   SequenceExpr $
                                     Sequence
                                       { sequenceUses = [],
                                         sequenceItems =
                                           [ -- let a = 10
                                             SequenceItemBindExpr $
                                               Bindings
                                                 { bindings = BindedSingle $ BindIdentifier $ Identifier "a",
                                                   bindingsBindType = Nothing,
                                                   bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10
                                                 },
                                             -- inner loop remapped in while
                                             SequenceItemExpr $
                                               WhileTerm $
                                                 While
                                                   { whileCondition = ValueLiteral $ Boolean True,
                                                     whileExpr = innerLoopBody
                                                   }
                                           ],
                                         -- return a
                                         sequenceEndExpr = Just $ Return $ Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a"
                                       }
                               }
                     }
                 )

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