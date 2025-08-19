module Move.Translations.ShadowingSpec (spec) where

import Control.Exception (evaluate)
import Data.Map qualified as Map
import Move.AST
import Move.Translations.Shadowing (generateUnshadowedName, getBindIdentifiers, getUnshadowedName, removeShadowing)
import Test.Hspec

testGetUnshadowedName :: Spec
testGetUnshadowedName = describe "Tests the function `getUnshadowedName`" $ do
  it "Throws and error when no scopes exist" $ do
    evaluate (getUnshadowedName (Identifier "a") []) `shouldThrow` anyException

  it "Throws and error when no unshadowed name is found in scopes" $ do
    evaluate
      ( getUnshadowedName
          (Identifier "a")
          [ Map.fromList [(Identifier "b", Identifier "b_inner")]
          ]
      )
      `shouldThrow` anyException

  it "Returns the topmost unshadowed name" $
    do
      getUnshadowedName
        (Identifier "a")
        [ Map.fromList [(Identifier "b", Identifier "b_inner")],
          Map.fromList [(Identifier "a", Identifier "a_inner"), (Identifier "b", Identifier "b")],
          Map.fromList [(Identifier "a", Identifier "a")]
        ]
      `shouldBe` Identifier "a_inner"

testGetBindIdentifiers :: Spec
testGetBindIdentifiers = describe "Tests the function `getBindIdentifiers`" $ do
  it "Returns all the binded identifiers given a single bind" $ do
    {- Code as follows:
      let MyStruct{a, b: b_alias} = e
    -}
    let bind =
          BindNamedStruct $
            BindedNamedStruct
              { bnsNameAccessChain = LocalNameAccessChain $ Identifier "MyStruct",
                bnsTypeArgs = [],
                bnsFields =
                  BindedFields
                    { hasPartialPattern = False,
                      bindedFields =
                        [ BindedField {bindFieldIdentifier = Identifier "a", bindFieldInnerBind = Nothing},
                          BindedField {bindFieldIdentifier = Identifier "b", bindFieldInnerBind = Just $ BindIdentifier $ Identifier "b_alias"}
                        ]
                    }
              }

    getBindIdentifiers bind `shouldBe` [Identifier "a", Identifier "b_alias"]

testGenerateUnshadowedName :: Spec
testGenerateUnshadowedName = describe "Tests for the function `generateUnshadowedName`" $ do
  it "Should append _inner until no shadowing" $ do
    let scopes =
          [ Map.fromList [(Identifier "a", Identifier "a_inner_inner")],
            Map.fromList [(Identifier "b", Identifier "b")],
            Map.fromList [(Identifier "a", Identifier "a_inner")],
            Map.fromList [(Identifier "a", Identifier "a")]
          ]

    -- Since a appears three times in the scope, append _inner three times
    generateUnshadowedName (Identifier "a") scopes `shouldBe` Identifier "a_inner_inner_inner"

testShadowingExpr :: Spec
testShadowingExpr = describe "Removing shadowing in an expression" $ do
  it "Appends _inner to a shadowing bind and subsequent usages" $ do
    {- Code as follows:
        {
            let x;
            {
                let x = x + 12;             -- Should be `let x_inner = x + 12`
                x = x * 2;                  -- Should be `x_inner = x_inner * 2`
            }

            x = x + 1                       -- No changes
        }
    -}

    let fromExpr =
          SequenceExpr $
            Sequence
              { sequenceUses = [],
                sequenceItems =
                  [ SequenceItemBindExpr $
                      Bindings
                        { bindings = [BindIdentifier $ Identifier "x"],
                          bindingsBindType = Nothing,
                          bindingsBindExpr = Nothing
                        },
                    -- Inner Sequence
                    SequenceItemExpr $
                      SequenceExpr $
                        Sequence
                          { sequenceUses = [],
                            sequenceItems =
                              [ SequenceItemBindExpr $
                                  Bindings
                                    { bindings = [BindIdentifier $ Identifier "x"],
                                      bindingsBindType = Nothing,
                                      bindingsBindExpr =
                                        Just $
                                          BinaryOpExprExpr $
                                            Add
                                              (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x")
                                              (ValueLiteral $ Numerical $ LiteralIntDec 12)
                                    },
                                SequenceItemExpr $
                                  AssignmentExpr $
                                    Assignment
                                      { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x",
                                        assignmentRight =
                                          BinaryOpExprExpr $
                                            Mult
                                              (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x")
                                              (ValueLiteral $ Numerical $ LiteralIntDec 2)
                                      }
                              ],
                            sequenceEndExpr = Nothing
                          }
                  ],
                sequenceEndExpr =
                  Just $
                    AssignmentExpr $
                      Assignment
                        { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x",
                          assignmentRight =
                            BinaryOpExprExpr $
                              Add
                                (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x")
                                (ValueLiteral $ Numerical $ LiteralIntDec 1)
                        }
              }

    let toExpr =
          SequenceExpr $
            Sequence
              { sequenceUses = [],
                sequenceItems =
                  [ SequenceItemBindExpr $
                      Bindings
                        { bindings = [BindIdentifier $ Identifier "x"],
                          bindingsBindType = Nothing,
                          bindingsBindExpr = Nothing
                        },
                    -- Inner Sequence
                    SequenceItemExpr $
                      SequenceExpr $
                        Sequence
                          { sequenceUses = [],
                            sequenceItems =
                              [ SequenceItemBindExpr $
                                  Bindings
                                    { bindings = [BindIdentifier $ Identifier "x_inner"],
                                      bindingsBindType = Nothing,
                                      bindingsBindExpr =
                                        Just $
                                          BinaryOpExprExpr $
                                            Add
                                              (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x")
                                              (ValueLiteral $ Numerical $ LiteralIntDec 12)
                                    },
                                SequenceItemExpr $
                                  AssignmentExpr $
                                    Assignment
                                      { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x_inner",
                                        assignmentRight =
                                          BinaryOpExprExpr $
                                            Mult
                                              (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x_inner")
                                              (ValueLiteral $ Numerical $ LiteralIntDec 2)
                                      }
                              ],
                            sequenceEndExpr = Nothing
                          }
                  ],
                sequenceEndExpr =
                  Just $
                    AssignmentExpr $
                      Assignment
                        { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x",
                          assignmentRight =
                            BinaryOpExprExpr $
                              Add
                                (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "x")
                                (ValueLiteral $ Numerical $ LiteralIntDec 1)
                        }
              }

    removeShadowing fromExpr [Map.empty] `shouldBe` toExpr

spec :: Spec
spec = do
  testGetUnshadowedName
  testGetBindIdentifiers
  testGenerateUnshadowedName
  testShadowingExpr