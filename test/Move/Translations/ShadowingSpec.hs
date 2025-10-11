module Move.Translations.ShadowingSpec (spec) where

import Control.Exception (evaluate)
import Data.Map qualified as Map
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Shadowing (generateUnshadowedName, getUnshadowedName, removeShadowingInRoot, removeShadowingInSequence)
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

testRemoveShadowingInSequence :: Spec
testRemoveShadowingInSequence = describe "Tests for the function `removeShadowingInSequence`" $ do
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

    let fromSequence =
          Sequence
            { sequenceUses = [],
              sequenceItems =
                [ SequenceItemBindExpr $
                    Bindings
                      { bindings = BindedSingle $ BindIdentifier $ Identifier "x",
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
                                  { bindings = BindedSingle $ BindIdentifier $ Identifier "x",
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

    let toSequence =
          Sequence
            { sequenceUses = [],
              sequenceItems =
                [ SequenceItemBindExpr $
                    Bindings
                      { bindings = BindedSingle $ BindIdentifier $ Identifier "x",
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
                                  { bindings = BindedSingle $ BindIdentifier $ Identifier "x_inner",
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

    removeShadowingInSequence fromSequence [Map.empty] `shouldBe` toSequence

testRemoveShadowingInRoot :: Spec
testRemoveShadowingInRoot = describe "Tests for the function `removeShadowingInRoot`" $ do
  it "Collects module constants, function parameters, and removes shadowing" $ do
    let fromModuleStr =
          "module NamedAddr::TestingLocalState {\n"
            ++ " const MY_CONST: u64 = 0;\n"
            ++ " public fun shadowing(x: u64): u64 {\n"
            ++ "   let x = 5;\n"
            ++ "   let y = 0;\n" --   {x: x}
            ++ "   {\n" --   {y: y, x: x}
            ++ "     x = x + 3;\n"
            ++ "     let x = x + 1;\n" --   -> x
            ++ "     x = x + 10;\n" --   Shadowing here                                        [{x: x_inner}, ..]
            ++ "     let x = true;\n" --   -> x_inner
            ++ "     {\n" --   A rebinding can change the type of x        -> x_inner
            ++ "       let x = 10;\n"
            ++ "       x = 50;\n" --   -> x_inner_inner                [{x: x_inner_inner}, ..]
            ++ "       let y = 10;\n" --   -> x_inner_inner
            ++ "       let MY_CONST = MY_CONST + 3;\n" --   -> y_inner                      [{y: y_inner, x: x_inner_inner}, ..]
            ++ "       MY_CONST = MY_CONST + 1;\n" --   -> let MY_CONST_inner = MY_CONST + 3             [{MY_CONST, MY_CONST_inner, y: y_inner, x: x_inner_inner}, ..]
            -- Note: This is actually invalid in Move, since it probably clashes with a constant name
            -- The compile-time error is "Unexpected assignment of module access without fields outside of a spec context"
            ++ "     }\n" --   -> MY_CONST_inner = MY_CONST_inner + 1
            ++ "   };\n"
            ++ "   x\n"
            ++ " }" --   x == 8
            ++ "}"

    let toModuleStr =
          "module NamedAddr::TestingLocalState {\n"
            ++ " const MY_CONST: u64 = 0;"
            ++ " public fun shadowing(x: u64): u64 {\n"
            ++ "   let x = 5;\n"
            ++ "   let y = 0;\n"
            ++ "   {\n"
            ++ "     x = x + 3;\n"
            ++ "     let x_inner = x + 1;\n"
            ++ "     x_inner = x_inner + 10;\n"
            ++ "     let x_inner = true;\n"
            ++ "     {\n"
            ++ "       let x_inner_inner = 10;\n"
            ++ "       x_inner_inner = 50;\n"
            ++ "       let y_inner = 10;\n"
            ++ "       let MY_CONST_inner = MY_CONST + 3;\n"
            ++ "       MY_CONST_inner = MY_CONST_inner + 1;\n"
            ++ "     }\n"
            ++ "   };\n"
            ++ "   x\n"
            ++ " }"
            ++ "}"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    removeShadowingInRoot fromModule `shouldBe` toModule

spec :: Spec
spec = do
  testGetUnshadowedName
  testGenerateUnshadowedName
  testRemoveShadowingInSequence
  testRemoveShadowingInRoot