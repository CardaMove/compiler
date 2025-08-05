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
  testScan "module foo::baz { struct A has copy {} struct B{x: u64, y: bool} } " $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelNamedStruct $ NamedStruct {
              namedStructIdentifier = Identifier "A",
              namedStructAbilities = [Copy],
              namedStructFields = []
            },
            TopLevelNamedStruct $ NamedStruct {
              namedStructIdentifier = Identifier "B",
              namedStructAbilities = [],
              namedStructFields = [
                NamedField {
                  fieldIdentifier = Identifier "x",
                  fieldType = Type (Identifier "u64") []
                },
                NamedField {
                  fieldIdentifier = Identifier "y",
                  fieldType = Type (Identifier "bool") []
              }]
            }
          ]
      }

testParsePositionalStruct :: Spec
testParsePositionalStruct = describe "Parse module with positional structs" $ do
  testScan "module foo::baz { struct A has copy; struct B(A, bool) has copy, drop; } " $
    Module
      { moduleAddress = NamedAddress $ Identifier "foo",
        moduleIdentifier = Identifier "baz",
        moduleTopLevels =
          [ TopLevelPositionalStruct $ PositionalStruct {
              positionalStructIdentifier = Identifier "A",
              positionalStructAbilities = [Copy],
              positionalStructFields = []
            },
            TopLevelPositionalStruct $ PositionalStruct {
              positionalStructIdentifier = Identifier "B",
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
  testScan $ "module foo::baz { " ++
    "public(friend) entry fun my_func<A, B>(a: A, b: B, c: u64): u64 acquires MyResource<A, u64> {}" ++
    "native public fun empty<Element>(): vector<Element>;" ++
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
              TypeParameter { typeIdentifier = Identifier "A", typeConstraints = () },
              TypeParameter { typeIdentifier = Identifier "B", typeConstraints = () }
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
            functionTypeParameters = [TypeParameter {typeIdentifier = Identifier "Element", typeConstraints = ()}],
            functionParameters = [],
            functionReturnType = Just $ Type (Identifier "vector") [Type (Identifier "Element") []],
            functionAcquires = [],
            functionBody = ()
          }
        ]
      }

{- testParseModuleKeyAbility :: Spec
testParseModuleKeyAbility = describe "Parse module with one empty struct with key ability" $ do
  testScan "module foo::baz { struct K has key {} }" $
    Module
      { moduleAddress = "foo",
        moduleIdentifier = "baz",
        moduleTopLevels =
          [ TopLevelStruct $
              Struct
                { structIdentifier = "K",
                  structFields = [],
                  structAbilities = [Key]
                }
          ]
      } -}

spec :: Spec
spec = do
  testParseEmptyModule
  testParseModuleUse
  testParseModuleFriend
  testParseNamedStruct
  testParsePositionalStruct
  testParseFunction
  -- testParseModuleKeyAbility
