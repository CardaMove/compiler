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
      { moduleAddress = NamedAddress (Identifier "foo"),
        moduleIdentifier = Identifier "bar",
        moduleTopLevels = []
      }

  -- Numerical address hex
  testScan "module 0xCAFFE::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress (LiteralIntHex "0xCAFFE"),
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

  -- Numerical address decimal with separators
  testScan "module 123_456u64::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress (LiteralIntDec 123456),
        moduleIdentifier = Identifier "fizzbuzz",
        moduleTopLevels = []
      }

testParseModuleUse :: Spec
testParseModuleUse = describe "Parse a module with use keywords" $ do
  testScan "module 0x42::answer { use std::vector; use 0x42::my_module as my_alias; }" $
    Module {
      moduleAddress = NumericalAddress (LiteralIntHex "0x42"),
      moduleIdentifier = Identifier "answer",
      moduleTopLevels = [
        TopLevelUse (Use {
          useAddress = NamedAddress (Identifier "std"),
          useName = Identifier "vector",
          useAlias = Nothing
        }),
        TopLevelUse (Use {
          useAddress = NumericalAddress (LiteralIntHex "0x42"),
          useName = Identifier "my_module",
          useAlias = Just (Identifier "my_alias")
        })
      ]
    }

testParseModuleFriend :: Spec
testParseModuleFriend = describe "Parse a module with friends" $ do
  testScan "module 0x42::answer { friend 0x42::b; friend aliased_friend; }" $
    Module {
      moduleAddress = NumericalAddress (LiteralIntHex "0x42"),
      moduleIdentifier = Identifier "answer",
      moduleTopLevels = [
        TopLevelFriend (Friend {
          friendAddress = Just (NumericalAddress (LiteralIntHex "0x42")),
          friendName = Identifier "b"
        }),
        TopLevelFriend (Friend {
          friendAddress = Nothing,
          friendName = Identifier "aliased_friend"
        })
      ]
    }

{- testParseModuleOneEmptyStruct :: Spec
testParseModuleOneEmptyStruct = describe "Parse module with one empty struct" $ do
  testScan "module foo::baz { struct A {} } " $
    Module
      { moduleAddress = "foo",
        moduleIdentifier = "baz",
        moduleTopLevels =
          [ TopLevelStruct $
              Struct
                { structIdentifier = "A",
                  structFields = [],
                  structAbilities = []
                }
          ]
      }

testParseModuleKeyAbility :: Spec
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
  -- testParseModuleOneEmptyStruct
  -- testParseModuleKeyAbility
