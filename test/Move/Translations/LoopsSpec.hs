module Move.Translations.LoopsSpec (spec) where

import Move.Translations.Loops (mapLoopsToWhile)
import Move.AST
import Test.Hspec

testLoopsToWhile :: Spec
testLoopsToWhile = describe "Translating a loop into a while" $ do
    it "Translates a Loop Expr" $ do
        let body = SequenceExpr $ Sequence {
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

        (mapLoopsToWhile $ Loop body) `shouldBe` (WhileTerm $ While {
            whileCondition = ValueLiteral $ Boolean True,
            whileExpr = body
        })

spec :: Spec
spec = do
    testLoopsToWhile