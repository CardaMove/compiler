module Move.Translations.LoopsSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Loops (translateLoopsToWhile, translateWhilesToFunctionsInModule)
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

testTranslateWhilesToFunctionsInModule :: Spec
testTranslateWhilesToFunctionsInModule = describe "Tests for the function `translateWhilesToFunctionsInModule`" $ do
  it "Translates a while loop into a function declaration" $ do
    let fromModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " public fun test(x: u64) {\n"
            ++ "   let a = 12;\n"
            ++ "   while(a < x){\n"
            ++ "     let b = 9;\n"
            ++ "     a = a + b;\n"
            ++ "   }\n"
            ++ " }\n"
            ++ "}"

    let toModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " fun mapped_while_0(a: &mut u256, x: &mut u64) {\n"
            ++
            -- The body of the while is inside a Sequence, with the end expression as the recursive call
            -- Note: a comparison between two different numeric types is not allowed in Move
            "   if (*a < *x) {\n"
            ++ "     {\n"
            ++ "       let b = 9;\n"
            ++ "       *a = *a + b;\n"
            ++ "     };\n"
            ++ "     mapped_while_0(a, x)\n"
            ++ "   }\n"
            ++ " }\n"
            ++
            -- The original function calls the newly created one
            " public fun test(x: u64) {\n"
            ++ "   let a = 12;\n"
            ++ "   mapped_while_0(&mut a, &mut x)\n"
            ++ " }\n"
            ++ "}"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    translateWhilesToFunctionsInModule fromModule `shouldBe` toModule

  it "Translates a while loop with break and continue inside" $ do
    let fromModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " public fun whileLoop(n: u64): u64 {\n"
            ++ "   let sum = 0;\n"
            ++ "   let i = 1;\n"
            ++ "   while (i <= n) {\n"
            ++ "     let inner = 42;\n"
            ++ "     sum = sum + i;\n"
            ++ "     if (i == 100) {\n"
            ++ "       break;\n"
            ++
            -- Useless, but other instructions can still be added after a break
            "       let a = 12;\n"
            ++ "     };\n"
            ++ "     if (n == 50) {\n"
            ++ "       continue;\n"
            ++ "     };\n"
            ++ "     let (a, b): (u64, u64);\n"
            ++ "     inner + 1\n"
            ++ "   };\n"
            ++ "   sum\n"
            ++ " }\n"
            ++ "}"

    let toModuleStr =
          "module NamedAddr::TraversalUtilsSpec {\n"
            ++ " fun mapped_while_0(i: &mut u256, n: &mut u64, sum: &mut u256){\n"
            ++ "   if(*i <= *n){\n"
            ++ "     {\n"
            ++ "       let break_hit: bool = false;\n"
            ++ "       let continue_hit: bool = false;\n"
            ++ "       let inner = 42;\n"
            ++ "       *sum = *sum + *i;\n"
            ++ "       if (*i == 100) {\n"
            ++ "         break_hit = true;\n"
            ++ "         if (!break_hit && !continue_hit){\n"
            ++ "           let a = 12;\n"
            ++ "         };\n"
            ++ "       };\n"
            ++ "       if (!break_hit && !continue_hit){\n"
            ++ "         if (*n == 50) {\n"
            ++ "           continue_hit = true;\n"
            ++ "         };\n"
            ++ "         if (!break_hit && !continue_hit) {\n"
            ++ "           let (a, b): (u64, u64);\n"
            ++ "         };\n"
            ++ "       };\n"
            ++
            -- This end expression of a sequence needs to be wrapped
            "       if (!break_hit && !continue_hit) inner + 1\n"
            ++ "     };\n"
            ++
            -- The recursive call needs to be wrapped in an if-then
            "     if (!break_hit) mapped_while_0(i, n, sum)\n"
            ++ "   }\n"
            ++ " }\n"
            ++ "\n"
            ++ " public fun whileLoop(n: u64): u64 {\n"
            ++ "   let sum = 0;\n"
            ++ "   let i = 1;\n"
            ++ "   mapped_while_0(&mut i, &mut n, &mut sum);\n"
            ++ "   sum\n"
            ++ " }\n"
            ++ "}"

    let fromModule = parse $ scan fromModuleStr
    let toModule = parse $ scan toModuleStr

    translateWhilesToFunctionsInModule fromModule `shouldBe` toModule

spec :: Spec
spec = do
  testLoopsToWhile
  testTranslateWhilesToFunctionsInModule