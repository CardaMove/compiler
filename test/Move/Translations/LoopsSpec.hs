module Move.Translations.LoopsSpec (spec) where

import Move.Translations.Loops (mapLoopsToWhile)
import Move.AST
import Test.Hspec

testLoopsToWhile :: Spec
testLoopsToWhile = describe "Translating a loop into a while" $ do
    it "Translates a Loop Expr" $ do
        let loopBody = SequenceExpr $ Sequence {
            sequenceUses = [],
            sequenceItems = [
                -- let a = 10
                SequenceItemBindExpr $ Bindings {
                    bindings = [
                        BindIdentifier $ Identifier "a"
                    ],
                    bindingsBindType = Nothing,
                    bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10
                }
            ],
            -- return a
            sequenceEndExpr = Just $ Return $ Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a"
        }

        (mapLoopsToWhile $ Loop loopBody) `shouldBe` (WhileTerm $ While {
            whileCondition = ValueLiteral $ Boolean True,
            whileExpr = loopBody
        })


    it "Translates nested Loop expressions" $ do
        let innerLoopBody = SequenceExpr $ Sequence {
            sequenceUses = [],
            sequenceItems = [],
            -- a = a + 1
            sequenceEndExpr = Just $ AssignmentExpr $ Assignment {
                assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a",
                assignmentRight = BinaryOpExprExpr $ Add
                    (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a")
                    (ValueLiteral $ Numerical $ LiteralIntDec 1)
            }
        }

        let functionBody = Sequence {
            sequenceUses = [],
            sequenceItems = [],
            -- outer loop
            sequenceEndExpr = Just $ Loop $ SequenceExpr $ Sequence {
                sequenceUses = [],
                sequenceItems = [
                    -- let a = 10
                    SequenceItemBindExpr $ Bindings {
                        bindings = [
                            BindIdentifier $ Identifier "a"
                        ],
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

        (mapLoopsToWhile $ functionBody) `shouldBe` (Sequence {
            sequenceUses = [],
            sequenceItems = [],
            -- outer loop remapped in while
            sequenceEndExpr = Just $ WhileTerm $ While {
                whileCondition = ValueLiteral $ Boolean True,
                whileExpr = SequenceExpr $ Sequence {
                    sequenceUses = [],
                    sequenceItems = [
                        -- let a = 10
                        SequenceItemBindExpr $ Bindings {
                            bindings = [
                                BindIdentifier $ Identifier "a"
                            ],
                            bindingsBindType = Nothing,
                            bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10
                        },
                        -- inner loop remapped in while
                        SequenceItemExpr $ WhileTerm $ While {
                            whileCondition = ValueLiteral $ Boolean True,
                            whileExpr = innerLoopBody
                        }
                    ],
                    -- return a
                    sequenceEndExpr = Just $ Return $ Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a"
                }
            }
        })

spec :: Spec
spec = do
    testLoopsToWhile