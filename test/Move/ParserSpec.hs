{-# LANGUAGE OverloadedStrings #-}

module Move.ParserSpec (spec) where

import Move.AST
import Move.Lexer
import Move.Parser
import Test.Hspec

testScan :: String -> Module -> SpecWith ()
testScan str ast = it str $ parse (scan str) `shouldBe` ast

testParseEmptyModule :: Spec
testParseEmptyModule = describe "Parse an empty module" $ do
  -- Named address
  testScan "module foo::bar {}" $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "bar",
        moduleTopLevels = []
      }

  -- Numerical address hex
  testScan "module 0xCAFFE::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress $ LiteralIntHex "0xCAFFE",
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

  -- Numerical address decimal with separators
  testScan "module 123_456u64::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress $ LiteralIntDec 123456,
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

testParseModuleUse :: Spec
testParseModuleUse = describe "Parse a module with use keywords" $ do
  testScan "module 0x42::answer { use std::vector; use 0x42::my_module as my_alias; }" $
    Module {
      moduleAddress = NumericalAddress $ LiteralIntHex "0x42",
      moduleIdentifier = Identifier "answer",
      moduleTopLevels = [
        TopLevelUse (Use {
          useAddress = NamedAddress $ Identifier "std",
          useName = Identifier "vector",
          useAlias = Nothing
        }),
        TopLevelUse (Use {
          useAddress = NumericalAddress $ LiteralIntHex "0x42",
          useName = Identifier "my_module",
          useAlias = Just $ Identifier "my_alias"
        })
      ]
    }

testParseModuleFriend :: Spec
testParseModuleFriend = describe "Parse a module with friends" $ do
  testScan "module 0x42::answer { friend 0x42::b; friend aliased_friend; }" $
    Module {
      moduleAddress = NumericalAddress $ LiteralIntHex "0x42",
      moduleIdentifier = Identifier "answer",
      moduleTopLevels = [
        TopLevelFriend $ Friend {
          friendAddress = Just (NumericalAddress $ LiteralIntHex "0x42"),
          friendName = Identifier "b"
        },
        TopLevelFriend $ Friend {
          friendAddress = Nothing,
          friendName = Identifier "aliased_friend"
        }
      ]
    }

testParseNamedStruct :: Spec
testParseNamedStruct = describe "Parse module with named structs" $ do
  testScan "module foo::baz { struct A has copy {} struct B<TypeParam>{x: u64, y: bool, c: TypeParam} } " $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelNamedStruct $ NamedStruct {
              namedStructIdentifier = Identifier "A",
              namedStructTypeParameters = [],
              namedStructAbilities = [Copy],
              namedStructFields = []
            },
            TopLevelNamedStruct $ NamedStruct {
              namedStructIdentifier = Identifier "B",
              namedStructTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "TypeParam", typeConstraints = []}],
              namedStructAbilities = [],
              namedStructFields = [
                NamedField {
                  fieldIdentifier = Identifier "x",
                  fieldType = Type (Identifier "u64") []
                },
                NamedField {
                  fieldIdentifier = Identifier "y",
                  fieldType = Type (Identifier "bool") []
                },
                NamedField {
                  fieldIdentifier = Identifier "c",
                  fieldType = Type (Identifier "TypeParam") []
                }
              ]
            }
          ]
      }

testParsePositionalStruct :: Spec
testParsePositionalStruct = describe "Parse module with positional structs" $ do
  testScan "module foo::baz { struct A<phantom TypeParam, TypeParam2> has copy; struct B(A, bool) has copy, drop; } " $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels = [
          TopLevelPositionalStruct $ PositionalStruct {
            positionalStructIdentifier = Identifier "A",
            positionalStructTypeParameters = [
              TypeParameter {typeParameterIsPhantom = True, typeIdentifier = Identifier "TypeParam", typeConstraints = []},
              TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "TypeParam2", typeConstraints = []}
            ],
            positionalStructAbilities = [Copy],
            positionalStructFields = []
          },
          TopLevelPositionalStruct $ PositionalStruct {
            positionalStructIdentifier = Identifier "B",
            positionalStructTypeParameters = [],
            positionalStructAbilities = [Copy, Drop],
            positionalStructFields = [
              PositionalField $ Type (Identifier "A") [],
              PositionalField $ Type (Identifier "bool") []
            ]
          }
        ]
      }

testParseFunction :: Spec
testParseFunction = describe "Parse module with function declaration" $ do
  testScan $ "module foo::baz {\n" ++
    "public(friend) entry fun my_func<A, B>(a: A, b: B, c: u64): u64 acquires MyResource<A, u64> {}\n" ++
    "native public fun empty<Element>(): vector<Element>;\n" ++
    " }"
  $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels = [
          TopLevelFunction $ Function {
            functionHasNativeModifier = False,
            functionVisibilityModifier = Just VisibilityModifierFriend,
            functionHasEntryModifier = True,
            functionName = Identifier "my_func",
            functionTypeParameters = [
              TypeParameter { typeParameterIsPhantom = False, typeIdentifier = Identifier "A", typeConstraints = [] },
              TypeParameter { typeParameterIsPhantom = False, typeIdentifier = Identifier "B", typeConstraints = [] }
            ],
            functionParameters = [
              Parameter { parameterIdentifier = Identifier "a", parameterType = Type (Identifier "A") [] },
              Parameter { parameterIdentifier = Identifier "b", parameterType = Type (Identifier "B") [] },
              Parameter { parameterIdentifier = Identifier "c", parameterType = Type (Identifier "u64") [] }
            ],
            functionReturnType = Just $ Type (Identifier "u64") [],
            functionAcquires = [
              Type (Identifier "MyResource") [Type (Identifier "A") [], Type (Identifier "u64") []]
            ],
            functionBody = ()
          },
          -- Native function
          TopLevelFunction $ Function {
            functionHasNativeModifier = True,
            functionVisibilityModifier = Just VisibilityModifierPublic,
            functionHasEntryModifier = False,
            functionName = Identifier "empty",
            functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "Element", typeConstraints = []}],
            functionParameters = [],
            functionReturnType = Just $ Type (Identifier "vector") [Type (Identifier "Element") []],
            functionAcquires = [],
            functionBody = ()
          }
        ]
      }

testParseAbilities :: Spec
testParseAbilities = describe "Parse structs with abilities and constraints" $ do
  testScan "module foo::baz { struct K<phantom T1: copy + drop, T2> has key {} }" $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelNamedStruct $ NamedStruct {
              namedStructIdentifier = Identifier "K",
              namedStructTypeParameters = [
                TypeParameter {
                  typeParameterIsPhantom = True,
                  typeIdentifier = Identifier "T1",
                  typeConstraints = [Copy, Drop]
                },
                TypeParameter {
                  typeParameterIsPhantom = False,
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
testParseConstants = describe "Parse modules with constant declarations" $ do
  testScan $ "module foo::baz {\n" ++
    "const C1: u64 = 1;\n" ++
    "const C2: u64 = 1 + 2 - 3;\n" ++
    "const C3: bool = false;\n" ++
    "const C4: address = @0xCAFFE;\n" ++
    "const C5: u64 = 1 + 2 * 3 - 4 | 2 << 4;\n" ++            -- Should parse as ((1 + (2 * 3)) - 4) | (2 << 4)
    -- It's correct that -1 would not parse, since Move does not have negative numeric values
    "const C6: u64 = 1 + *my_ref + move my_var;\n" ++           -- Should parse as ((-1) + (*my_ref)) + (move my_var). Not valid Move code, but test for expression
    "const MY_A: A = A { b: 10 };\n" ++                         -- Not valid Move code, but test for expression
    "const MY_POS: Pos = 0x42::another_module::Pos(12u64, false);\n" ++         -- Not valid Move code, but test for expression
    "}"
  $ Module
    { moduleAddress = NamedAddress $ Identifier "foo",
      moduleIdentifier = Identifier "baz",
      moduleTopLevels =
        [ TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C1",
            constantType = Type (Identifier "u64") [],
            constantExpression = ValueLiteral $ Numerical $ LiteralIntDec 1
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C2",
            constantType = Type (Identifier "u64") [],
            constantExpression = BinaryOpExprExpr $ Sub 
              (BinaryOpExprExpr $ Add (ValueLiteral $ Numerical $ LiteralIntDec 1) (ValueLiteral $ Numerical $ LiteralIntDec 2))
              (ValueLiteral $ Numerical $ LiteralIntDec 3)
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C3",
            constantType = Type (Identifier "bool") [],
            constantExpression = ValueLiteral $ Boolean False
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C4",
            constantType = Type (Identifier "address") [],
            constantExpression = ValueLiteral $ Address $ NumericalAddress $ LiteralIntHex "0xCAFFE"
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C5",
            constantType = Type (Identifier "u64") [],
            constantExpression = BinaryOpExprExpr $ BitwiseOr
              (BinaryOpExprExpr $ Sub
                (BinaryOpExprExpr $ Add 
                  (ValueLiteral $ Numerical $ LiteralIntDec 1)
                  (BinaryOpExprExpr $ Mult
                    (ValueLiteral $ Numerical $ LiteralIntDec 2)
                    (ValueLiteral $ Numerical $ LiteralIntDec 3)
                  )
                )
                (ValueLiteral $ Numerical $ LiteralIntDec 4)
              )
              (BinaryOpExprExpr $ ShiftLeft 
                (ValueLiteral $ Numerical $ LiteralIntDec 2)
                (ValueLiteral $ Numerical $ LiteralIntDec 4)
              )
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "C6",
            constantType = Type (Identifier "u64") [],
            constantExpression = BinaryOpExprExpr $ Add
              (BinaryOpExprExpr $ Add
                (ValueLiteral $ Numerical $ LiteralIntDec 1)
                (UnaryOpExpr $ Dereference $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "my_ref") -- FIXME: placeholder
              )
              (UnaryOpExpr $ MoveExpr $ Identifier "my_var")
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "MY_A",
            constantType = Type (Identifier "A") [],
            constantExpression = NamedStructExprExpr $ NamedStructExpr {
              nseNameAccessChain = LocalNameAccessChain $ Identifier "A",
              nseTypeArgs = [],
              nseFields = [NamedStructExprField { nsefIdentifier = Identifier "b", nsefExpr = Just $ ValueLiteral $ Numerical $ LiteralIntDec 10 }]
            }
          },
          TopLevelConstant $ Constant {
            constantIdentifier = Identifier "MY_POS",
            constantType = Type (Identifier "Pos") [],
            constantExpression = PositionalStructExprOrFunctionCallExpr $ PositionalStructExprOrFunctionCall {
              pseofcNameAccessChain = UnaliasedNameAccessChain (NumericalAddress $ LiteralIntHex "0x42") (Identifier "another_module") (Identifier "Pos"),
              pseofcTypeArgs = [],
              pseofcFields = [
                ValueLiteral $ Numerical $ LiteralIntDec 12,
                ValueLiteral $ Boolean False
              ]
            }
          }
        ]
    }
    

spec :: Spec
spec = do
  testParseEmptyModule
  testParseModuleUse
  testParseModuleFriend
  testParseNamedStruct
  testParsePositionalStruct
  testParseFunction
  testParseAbilities
  testParseConstants
