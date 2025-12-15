{-# LANGUAGE OverloadedStrings #-}

module Move.ParserSpec (spec) where

import Move.AST
import Move.Lexer
import Move.Parser
import Test.Hspec

testScan :: String -> Root -> SpecWith ()
testScan str ast = it (filter (/= '\n') str) $ do
  let tokens = scan str
  -- print tokens
  parse tokens `shouldBe` ast

testParseEmptyModule :: Spec
testParseEmptyModule = describe "Parse an empty module" $ do
  -- Named address
  testScan "module foo::bar {}" $ RModule $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "bar",
        moduleTopLevels = []
      }

  -- Numerical address hex
  testScan "module 0xCAFFE::fizzbuzz {}" $ RModule $
    Module
      { moduleAddress = NumericalAddress $ LiteralIntHex "0xCAFFE",
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

  -- Numerical address decimal with separators
  testScan "module 123_456u64::fizzbuzz {}" $ RModule $
    Module
      { moduleAddress = NumericalAddress $ LiteralIntDec 123456,
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

testParseModuleUse :: Spec
testParseModuleUse = describe "Parse a module with use keywords" $ do
  testScan
    ( "module 0x42::answer {\n"
        ++ "use std::vector;\n"
        ++ "use 0x42::my_module as my_alias;\n"
        ++ "use std::vector::empty as empty_vec;\n"
        ++ "use std::vector::{push_back, length as len, pop_back};\n"
        ++ "}"
    )
    $ RModule $ Module
      { moduleAddress = NumericalAddress $ LiteralIntHex "0x42",
        moduleIdentifier = Identifier "answer",
        moduleTopLevels =
          [ TopLevelUse
              ( Use
                  { useAddress = NamedAddress $ Identifier "std",
                    useIdentifier = Identifier "vector",
                    useAlias = Nothing,
                    useMembers = []
                  }
              ),
            TopLevelUse
              ( Use
                  { useAddress = NumericalAddress $ LiteralIntHex "0x42",
                    useIdentifier = Identifier "my_module",
                    useAlias = Just $ Identifier "my_alias",
                    useMembers = []
                  }
              ),
            TopLevelUse
              ( Use
                  { useAddress = NamedAddress $ Identifier "std",
                    useIdentifier = Identifier "vector",
                    useAlias = Nothing,
                    useMembers =
                      [ UseMember {useMemberIdentifier = Identifier "empty", useMemberUseAlias = Just $ Identifier "empty_vec"}
                      ]
                  }
              ),
            TopLevelUse
              ( Use
                  { useAddress = NamedAddress $ Identifier "std",
                    useIdentifier = Identifier "vector",
                    useAlias = Nothing,
                    useMembers =
                      [ UseMember {useMemberIdentifier = Identifier "push_back", useMemberUseAlias = Nothing},
                        UseMember {useMemberIdentifier = Identifier "length", useMemberUseAlias = Just $ Identifier "len"},
                        UseMember {useMemberIdentifier = Identifier "pop_back", useMemberUseAlias = Nothing}
                      ]
                  }
              )
          ]
      }

testParseModuleFriend :: Spec
testParseModuleFriend = describe "Parse a module with friends" $ do
  testScan "module 0x42::answer { friend 0x42::b; friend aliased_friend; }" $ RModule $
    Module
      { moduleAddress = NumericalAddress $ LiteralIntHex "0x42",
        moduleIdentifier = Identifier "answer",
        moduleTopLevels =
          [ TopLevelFriend $ Friend $ AliasedNameAccessChain (NumericalAddress $ LiteralIntHex "0x42") (Identifier "b"),
            TopLevelFriend $ Friend $ LocalNameAccessChain $ Identifier "aliased_friend"
          ]
      }

testParseNamedStruct :: Spec
testParseNamedStruct = describe "Parse module with named structs" $ do
  testScan "module foo::baz { struct A has copy {} struct B<TypeParam>{x: u64, y: bool, c: TypeParam} } " $ RModule $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelNamedStruct $
              NamedStruct
                { namedStructIdentifier = Identifier "A",
                  namedStructTypeParameters = [],
                  namedStructAbilities = [Copy],
                  namedStructFields = []
                },
            TopLevelNamedStruct $
              NamedStruct
                { namedStructIdentifier = Identifier "B",
                  namedStructTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "TypeParam", typeConstraints = []}],
                  namedStructAbilities = [],
                  namedStructFields =
                    [ NamedField
                        { fieldIdentifier = Identifier "x",
                          fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []
                        },
                      NamedField
                        { fieldIdentifier = Identifier "y",
                          fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") []
                        },
                      NamedField
                        { fieldIdentifier = Identifier "c",
                          fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "TypeParam") []
                        }
                    ]
                }
          ]
      }

testParsePositionalStruct :: Spec
testParsePositionalStruct = describe "Parse module with positional structs" $ do
  testScan "module foo::baz { struct A<phantom TypeParam, TypeParam2> has copy; struct B(A, bool) has copy, drop; } " $ RModule $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelPositionalStruct $
              PositionalStruct
                { positionalStructIdentifier = Identifier "A",
                  positionalStructTypeParameters =
                    [ TypeParameter {typeParameterIsPhantom = True, typeIdentifier = Identifier "TypeParam", typeConstraints = []},
                      TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "TypeParam2", typeConstraints = []}
                    ],
                  positionalStructAbilities = [Copy],
                  positionalStructFields = []
                },
            TopLevelPositionalStruct $
              PositionalStruct
                { positionalStructIdentifier = Identifier "B",
                  positionalStructTypeParameters = [],
                  positionalStructAbilities = [Copy, Drop],
                  positionalStructFields =
                    [ PositionalField $ TypeConstructor (LocalNameAccessChain $ Identifier "A") [],
                      PositionalField $ TypeConstructor (LocalNameAccessChain $ Identifier "bool") []
                    ]
                }
          ]
      }

testParseFunctionNoBody :: Spec
testParseFunctionNoBody =
  describe "Parse module with function declaration with no body"
    $ do
      testScan $
        "module foo::baz {\n"
          ++ "public(friend) entry fun my_func<A, B>(a: A, b: B, c: u64): u64 acquires MyResource {}\n"
          ++ "native public fun empty<Element>(): vector<Element>;\n"
          ++ " }"
    $ RModule $ Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierFriend,
                  functionHasEntryModifier = True,
                  functionName = Identifier "my_func",
                  functionTypeParameters =
                    [ TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "A", typeConstraints = []},
                      TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "B", typeConstraints = []}
                    ],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "a", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "A") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "b", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "B") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "c", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  functionAcquires =
                    [ LocalNameAccessChain $ Identifier "MyResource"
                    ],
                  functionBody =
                    Just $
                      Sequence
                        { sequenceUses = [],
                          sequenceItems = [],
                          sequenceEndExpr = Nothing
                        },
                  functionUUID = Nothing
                },
            -- Native function
            TopLevelFunction $
              Function
                { functionHasNativeModifier = True,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "empty",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "Element", typeConstraints = []}],
                  functionParameters = [],
                  functionReturnType = Just $ TypeConstructor (LocalNameAccessChain $ Identifier "vector") [TypeConstructor (LocalNameAccessChain $ Identifier "Element") []],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                }
          ]
      }

testParseAbilities :: Spec
testParseAbilities = describe "Parse structs with abilities and constraints" $ do
  testScan "module foo::baz { struct K<phantom T1: copy + drop, T2> has key {} }" $ RModule $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelNamedStruct $
              NamedStruct
                { namedStructIdentifier = Identifier "K",
                  namedStructTypeParameters =
                    [ TypeParameter
                        { typeParameterIsPhantom = True,
                          typeIdentifier = Identifier "T1",
                          typeConstraints = [Copy, Drop]
                        },
                      TypeParameter
                        { typeParameterIsPhantom = False,
                          typeIdentifier = Identifier "T2",
                          typeConstraints = []
                        }
                    ],
                  namedStructAbilities = [Key],
                  namedStructFields = []
                }
          ]
      }

testParseConstants :: Spec
testParseConstants =
  describe "Parse modules with constant declarations"
    $ do
      testScan
        ( "module foo::baz {\n"
            ++ "const C1: u64 = 1;\n"
            ++ "const C2: u64 = 1 + 2 - 3;\n"
            ++ "const C3: bool = false;\n"
            ++ "const C4: address = @0xCAFFE;\n"
            ++ "const C5: u64 = 1 + 2 * 3 - 4 | 2 << 4;\n"
            ++ "const C6: u64 = 1 + *my_ref + move my_var;\n" -- Should parse as ((1 + (2 * 3)) - 4) | (2 << 4)
            -- It's correct that -1 would not parse, since Move does not have negative numeric values
            ++ "const MY_A: A = A { b: 10 };\n" -- Should parse as ((-1) + (*my_ref)) + (move my_var). Not valid Move code, but test for expression
            ++ "const MY_POS: Pos = 0x42::another_module::Pos(12u64, false);\n" -- Not valid Move code, but test for expression
            ++ "}" -- Not valid Move code, but test for expression
        )
    $ RModule $ Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C1",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  constantExpression = ValueLiteral $ Numerical $ LiteralIntDec 1
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C2",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  constantExpression =
                    BinaryOpExprExpr $
                      Sub
                        (BinaryOpExprExpr $ Add (ValueLiteral $ Numerical $ LiteralIntDec 1) (ValueLiteral $ Numerical $ LiteralIntDec 2))
                        (ValueLiteral $ Numerical $ LiteralIntDec 3)
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C3",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") [],
                  constantExpression = ValueLiteral $ Boolean False
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C4",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "address") [],
                  constantExpression = ValueLiteral $ Address $ NumericalAddress $ LiteralIntHex "0xCAFFE"
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C5",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  constantExpression =
                    BinaryOpExprExpr $
                      BitwiseOr
                        ( BinaryOpExprExpr $
                            Sub
                              ( BinaryOpExprExpr $
                                  Add
                                    (ValueLiteral $ Numerical $ LiteralIntDec 1)
                                    ( BinaryOpExprExpr $
                                        Mult
                                          (ValueLiteral $ Numerical $ LiteralIntDec 2)
                                          (ValueLiteral $ Numerical $ LiteralIntDec 3)
                                    )
                              )
                              (ValueLiteral $ Numerical $ LiteralIntDec 4)
                        )
                        ( BinaryOpExprExpr $
                            ShiftLeft
                              (ValueLiteral $ Numerical $ LiteralIntDec 2)
                              (ValueLiteral $ Numerical $ LiteralIntDec 4)
                        )
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "C6",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  constantExpression =
                    BinaryOpExprExpr $
                      Add
                        ( BinaryOpExprExpr $
                            Add
                              (ValueLiteral $ Numerical $ LiteralIntDec 1)
                              (UnaryOpExpr $ Dereference $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "my_ref")
                        )
                        (UnaryOpExpr $ MoveExpr $ Identifier "my_var")
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "MY_A",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "A") [],
                  constantExpression =
                    NamedStructExprExpr $
                      NamedStructExpr
                        { nseNameAccessChain = LocalNameAccessChain $ Identifier "A",
                          nseTypeArgs = [],
                          nseFields = [NamedStructExprField {nsefIdentifier = Identifier "b", nsefExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10}]
                        }
                },
            TopLevelConstant $
              Constant
                { constantIdentifier = Identifier "MY_POS",
                  constantType = TypeConstructor (LocalNameAccessChain $ Identifier "Pos") [],
                  constantExpression =
                    PositionalStructExprOrFunctionCallExpr $
                      PositionalStructExprOrFunctionCall
                        { pseofcNameAccessChain = UnaliasedNameAccessChain (NumericalAddress $ LiteralIntHex "0x42") (Identifier "another_module") (Identifier "Pos"),
                          pseofcTypeArgs = [],
                          pseofcFields =
                            [ ValueLiteral $ Numerical $ LiteralIntDec 12,
                              ValueLiteral $ Boolean False
                            ]
                        }
                }
          ]
      }

testParseFunctionWithBody :: Spec
testParseFunctionWithBody =
  describe "Parse module with function declaration with body"
    $ do
      testScan
        ( "module foo::baz {\n"
            ++ "fun my_func(b: u64): u64 {\n"
            ++ "use std::vector;\n"
            ++ "let a: u64 = 6 * 7;\n"
            ++ "let sum = a + b;\n"
            ++ "let inner = if (sum > 10) { sum / 10 } else sum + 2;\n"
            ++ "inner\n"
            ++ "}\n}"
        )
    $ RModule $ Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Nothing,
                  functionHasEntryModifier = False,
                  functionName = Identifier "my_func",
                  functionTypeParameters = [],
                  functionParameters = [Parameter {parameterIdentifier = Identifier "b", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], parameterUUID = Nothing}],
                  functionReturnType = Just $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                  functionAcquires = [],
                  functionBody =
                    Just $
                      Sequence
                        { -- use ...
                          sequenceUses =
                            [ Use
                                { useAddress = NamedAddress $ Identifier "std",
                                  useIdentifier = Identifier "vector",
                                  useAlias = Nothing,
                                  useMembers = []
                                }
                            ],
                          sequenceItems =
                            [ -- let a = ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "a") Nothing,
                                    bindingsBindType = Just $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                                    bindingsBindExpr =
                                      Just $
                                        BinaryOpExprExpr $
                                          Mult
                                            (ValueLiteral $ Numerical $ LiteralIntDec 6)
                                            (ValueLiteral $ Numerical $ LiteralIntDec 7)
                                  },
                              -- let sum =
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "sum") Nothing,
                                    bindingsBindType = Nothing,
                                    bindingsBindExpr =
                                      Just $
                                        BinaryOpExprExpr $
                                          Add
                                            (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "a")
                                            (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "b")
                                  },
                              -- let inner =
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "inner") Nothing,
                                    bindingsBindType = Nothing,
                                    bindingsBindExpr =
                                      Just $
                                        IfThenElseTerm $
                                          IfThenElse
                                            { -- if (...)
                                              ifThenElseCondition =
                                                BinaryOpExprExpr $
                                                  Gt
                                                    (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "sum")
                                                    (ValueLiteral $ Numerical $ LiteralIntDec 10),
                                              -- if branch
                                              ifThenElseIfBranch =
                                                SequenceExpr $
                                                  Sequence
                                                    { sequenceUses = [],
                                                      sequenceItems = [],
                                                      sequenceEndExpr =
                                                        Just $
                                                          BinaryOpExprExpr $
                                                            Div
                                                              (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "sum")
                                                              (ValueLiteral $ Numerical $ LiteralIntDec 10)
                                                    },
                                              -- else branch (must include the + 2)
                                              ifThenElseElseBranch =
                                                Just $
                                                  BinaryOpExprExpr $
                                                    Add
                                                      (NameAccessChainExpr $ LocalNameAccessChain $ Identifier "sum")
                                                      (ValueLiteral $ Numerical $ LiteralIntDec 2)
                                            }
                                  }
                            ],
                          -- inner
                          sequenceEndExpr = Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "inner"
                        },
                  functionUUID = Nothing
                }
          ]
      }

testParseStructExpr :: Spec
testParseStructExpr =
  describe "Parse function with struct expressions"
    $ do
      testScan
        ( "module foo::baz {\n"
            ++ "fun my_func2(b: &mut B<bool>) {\n"
            ++
            -- Mutating a struct
            "*b = B { b: b.b + 2 };\n"
            ++ "let c = 0;\n"
            ++
            -- Creating a struct with reference and field name punning
            "let foo = C<bool> { a: 12, b: *b, c };\n"
            ++
            -- Nested pattern matching, copy keyword
            "let C { a: _, b: B<bool> { b: my_b }, c: _ } = copy foo;\n"
            ++
            -- Reference to a field
            "let c_ref: &u64 = &foo.c;\n"
            ++
            -- Updating a field
            "foo.c = foo.c + *c_ref;\n"
            ++
            -- Positional structs
            "let positional = PositionalStruct(12, 24);\n"
            ++
            -- Pattern matching with positional struct
            "let PositionalStruct(_, twenty_four) = positional;\n"
            ++
            -- Partial patterns and tuple
            "let C { a: my_a,.. } = foo;\n"
            ++ "let (PositionalStruct(twelve,..), another_42): (PositionalStruct, u64) = (positional, 42);\n"
            ++ "}\n}"
        )
    $ RModule $ Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Nothing,
                  functionHasEntryModifier = False,
                  functionName = Identifier "my_func2",
                  functionTypeParameters = [],
                  functionParameters = [Parameter {parameterIdentifier = Identifier "b", parameterType = TypeMutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "B") [TypeConstructor (LocalNameAccessChain $ Identifier "bool") []], parameterUUID = Nothing}],
                  functionReturnType = Nothing,
                  functionAcquires = [],
                  functionBody =
                    Just $
                      Sequence
                        { sequenceUses = [],
                          sequenceItems =
                            [ -- \*b = ...
                              SequenceItemExpr $
                                AssignmentExpr $
                                  Assignment
                                    { assignmentLeft = UnaryOpExpr $ Dereference $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "b",
                                      -- B { ...
                                      assignmentRight =
                                        NamedStructExprExpr $
                                          NamedStructExpr
                                            { nseNameAccessChain = LocalNameAccessChain $ Identifier "B",
                                              nseTypeArgs = [],
                                              nseFields =
                                                [ -- b: b.b + 2
                                                  NamedStructExprField
                                                    { nsefIdentifier = Identifier "b",
                                                      nsefExpr =
                                                        Just $
                                                          BinaryOpExprExpr $
                                                            Add
                                                              ( DotOrIndexChainExpr $
                                                                  DotAccess
                                                                    { dotAccessLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "b",
                                                                      dotAccessRight = Identifier "b"
                                                                    }
                                                              )
                                                              (ValueLiteral $ Numerical $ LiteralIntDec 2)
                                                    }
                                                ]
                                            }
                                    },
                              -- let c = 0
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "c") Nothing,
                                    bindingsBindType = Nothing,
                                    bindingsBindExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 0
                                  },
                              -- let foo =
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "foo") Nothing,
                                    bindingsBindType = Nothing,
                                    -- C<bool> {..
                                    bindingsBindExpr =
                                      Just $
                                        NamedStructExprExpr $
                                          NamedStructExpr
                                            { nseNameAccessChain = LocalNameAccessChain $ Identifier "C",
                                              nseTypeArgs = [TypeConstructor (LocalNameAccessChain $ Identifier "bool") []],
                                              nseFields =
                                                [ -- a: 12
                                                  NamedStructExprField
                                                    { nsefIdentifier = Identifier "a",
                                                      nsefExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 12
                                                    },
                                                  -- b: *b
                                                  NamedStructExprField
                                                    { nsefIdentifier = Identifier "b",
                                                      nsefExpr = Just $ UnaryOpExpr $ Dereference $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "b"
                                                    },
                                                  -- c
                                                  NamedStructExprField
                                                    { nsefIdentifier = Identifier "c",
                                                      nsefExpr = Nothing
                                                    }
                                                ]
                                            }
                                  },
                              -- let C { ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $
                                        BindNamedStruct $
                                          BindedNamedStruct
                                            { bnsNameAccessChain = LocalNameAccessChain $ Identifier "C",
                                              bnsTypeArgs = [],
                                              bnsFields =
                                                BindedFields
                                                  { hasPartialPattern = False,
                                                    bindedFields =
                                                      [ -- a: _
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "a",
                                                            -- _ is parsed as an identifier
                                                            bindFieldInnerBind = Just $ BindIdentifier (Identifier "_") Nothing,
                                                            bindedFieldUUID = Nothing
                                                          },
                                                        -- b: B<bool> ...
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "b",
                                                            -- _ is parsed as an identifier
                                                            bindFieldInnerBind =
                                                              Just $
                                                                BindNamedStruct $
                                                                  BindedNamedStruct
                                                                    { bnsNameAccessChain = LocalNameAccessChain $ Identifier "B",
                                                                      bnsTypeArgs = [TypeConstructor (LocalNameAccessChain $ Identifier "bool") []],
                                                                      bnsFields =
                                                                        BindedFields
                                                                          { hasPartialPattern = False,
                                                                            bindedFields =
                                                                              [ -- b: my_b
                                                                                BindedField
                                                                                  { bindFieldIdentifier = Identifier "b",
                                                                                    bindFieldInnerBind = Just $ BindIdentifier (Identifier "my_b") Nothing,
                                                                                    bindedFieldUUID = Nothing
                                                                                  }
                                                                              ]
                                                                          }
                                                                    },
                                                            bindedFieldUUID = Nothing
                                                          },
                                                        -- c: _
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "c",
                                                            bindFieldInnerBind = Just $ BindIdentifier (Identifier "_") Nothing,
                                                            bindedFieldUUID = Nothing
                                                          }
                                                      ]
                                                  }
                                            }
                                      ,
                                    bindingsBindType = Nothing,
                                    -- = copy foo
                                    bindingsBindExpr = Just $ UnaryOpExpr $ CopyExpr $ Identifier "foo"
                                  },
                              -- let c_ref: &u64 ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "c_ref") Nothing,
                                    bindingsBindType = Just $ TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "u64") [],
                                    -- = &foo.c
                                    bindingsBindExpr =
                                      Just $
                                        UnaryOpExpr $
                                          ImmutableReference $
                                            DotOrIndexChainExpr $
                                              DotAccess
                                                { dotAccessLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "foo",
                                                  dotAccessRight = Identifier "c"
                                                }
                                  },
                              -- foo.c = ...
                              SequenceItemExpr $
                                AssignmentExpr $
                                  Assignment
                                    { -- foo.c
                                      assignmentLeft =
                                        DotOrIndexChainExpr $
                                          DotAccess
                                            { dotAccessLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "foo",
                                              dotAccessRight = Identifier "c"
                                            },
                                      -- = foo.c + *c_ref
                                      assignmentRight =
                                        BinaryOpExprExpr $
                                          Add
                                            ( DotOrIndexChainExpr $
                                                DotAccess
                                                  { dotAccessLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "foo",
                                                    dotAccessRight = Identifier "c"
                                                  }
                                            )
                                            (UnaryOpExpr $ Dereference $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "c_ref")
                                    },
                              -- let positional = ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier (Identifier "positional") Nothing,
                                    bindingsBindType = Nothing,
                                    -- = PositionalStruct(12, 24)
                                    bindingsBindExpr =
                                      Just $
                                        PositionalStructExprOrFunctionCallExpr $
                                          PositionalStructExprOrFunctionCall
                                            { pseofcNameAccessChain = LocalNameAccessChain $ Identifier "PositionalStruct",
                                              pseofcTypeArgs = [],
                                              pseofcFields =
                                                [ ValueLiteral $ Numerical $ LiteralIntDec 12,
                                                  ValueLiteral $ Numerical $ LiteralIntDec 24
                                                ]
                                            }
                                  },
                              -- let PositionalStruct(...;
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $
                                        BindPositionalStruct $
                                          BindedPositionalStruct
                                            { bpsNameAccessChain = LocalNameAccessChain $ Identifier "PositionalStruct",
                                              bpsTypeArgs = [],
                                              bpsFields =
                                                BindedFields
                                                  { hasPartialPattern = False,
                                                    bindedFields =
                                                      [ -- _
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "_",
                                                            bindFieldInnerBind = Nothing,
                                                            bindedFieldUUID = Nothing
                                                          },
                                                        -- twenty_four
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "twenty_four",
                                                            bindFieldInnerBind = Nothing,
                                                            bindedFieldUUID = Nothing
                                                          }
                                                      ]
                                                  }
                                            }
                                      ,
                                    bindingsBindType = Nothing,
                                    -- = positional
                                    bindingsBindExpr = Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "positional"
                                  },
                              -- let C { ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ 
                                        BindNamedStruct $
                                          BindedNamedStruct
                                            { bnsNameAccessChain = LocalNameAccessChain $ Identifier "C",
                                              bnsTypeArgs = [],
                                              bnsFields =
                                                BindedFields
                                                  { -- ..
                                                    hasPartialPattern = True,
                                                    bindedFields =
                                                      [ -- a: my_a
                                                        BindedField
                                                          { bindFieldIdentifier = Identifier "a",
                                                            bindFieldInnerBind = Just $ BindIdentifier (Identifier "my_a") Nothing,
                                                            bindedFieldUUID = Nothing
                                                          }
                                                      ]
                                                  }
                                            }
                                      ,
                                    bindingsBindType = Nothing,
                                    -- = foo
                                    bindingsBindExpr = Just $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "foo"
                                  },
                              -- let ( ...
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedTuple $
                                      [ -- PositionalStruct(twelve,..)
                                        BindPositionalStruct $
                                          BindedPositionalStruct
                                            { bpsNameAccessChain = LocalNameAccessChain $ Identifier "PositionalStruct",
                                              bpsTypeArgs = [],
                                              bpsFields =
                                                BindedFields
                                                  { hasPartialPattern = True,
                                                    bindedFields =
                                                      [ BindedField {bindFieldIdentifier = Identifier "twelve", bindFieldInnerBind = Nothing, bindedFieldUUID = Nothing}
                                                      ]
                                                  }
                                            },
                                        -- another_42)
                                        BindIdentifier (Identifier "another_42") Nothing
                                      ],
                                    -- : (PositionalSttruct, u64)
                                    bindingsBindType =
                                      Just $
                                        TypeTuple
                                          [ TypeConstructor (LocalNameAccessChain $ Identifier "PositionalStruct") [],
                                            TypeConstructor (LocalNameAccessChain $ Identifier "u64") []
                                          ],
                                    -- = (positional, 42)
                                    bindingsBindExpr =
                                      Just $
                                        CommaExpr
                                          [ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "positional",
                                            ValueLiteral $ Numerical $ LiteralIntDec 42
                                          ]
                                  }
                            ],
                          -- No end expression
                          sequenceEndExpr = Nothing
                        },
                  functionUUID = Nothing
                }
          ]
      }

-- The '>>' operator made the parser fail in the case of nested type arguments.
-- For more details, see Parser.y 
testParseTypeArguments :: Spec
testParseTypeArguments = describe "Parse function with type arguments" $ do
  it "Parse function with >>" $ do
    fromModuleStr <- readFile "test/Move/files/ParserSpec_0.move"
    toModuleStr <- readFile "test/Move/files/ParserSpec_1.txt"
    let toModule = read toModuleStr :: Root

    let tokens = scan fromModuleStr
    parse tokens `shouldBe` toModule


spec :: Spec
spec = do
  testParseEmptyModule
  testParseModuleUse
  testParseModuleFriend
  testParseNamedStruct
  testParsePositionalStruct
  testParseFunctionNoBody
  testParseAbilities
  testParseConstants
  testParseFunctionWithBody
  testParseStructExpr
  testParseTypeArguments
