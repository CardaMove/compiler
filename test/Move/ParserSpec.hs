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
        moduleIdentifier = Identifier "bar" --,
        -- moduleTopLevels = []
      }
      
  -- Numerical address hex
  testScan "module 0xCAFFE::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress (LiteralIntHex "0xCAFFE"),
        moduleIdentifier = Identifier "fizzbuzz" --,
        -- moduleTopLevels = []
      }

  -- Numerical address decimal with separators
  testScan "module 123_456u64::fizzbuzz {}" $
    Module
      { moduleAddress = NumericalAddress (LiteralIntDec 123456),
        moduleIdentifier = Identifier "fizzbuzz" --,
        -- moduleTopLevels = []
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
  -- testParseModuleOneEmptyStruct
  -- testParseModuleKeyAbility
