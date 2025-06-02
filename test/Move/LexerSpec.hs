module Move.LexerSpec (spec) where

import Move.Lexer
import Move.Token
import Test.Hspec

testScan :: String -> [Token] -> SpecWith ()
testScan str toks = it str $ scan str `shouldBe` toks

testScanBraces :: Spec
testScanBraces = describe "Parse {}" $ do
  testScan "{}" [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "{ }" [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "{ } " [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan " { }" [TokenSeparatorLBrace, TokenSeparatorRBrace]

testScanModule :: Spec
testScanModule = describe "Parse module" $ do
  testScan "module" [TokenKeywordModule]
  testScan "module " [TokenKeywordModule]
  testScan "module {" [TokenKeywordModule, TokenSeparatorLBrace]
  testScan "module culo {}" [TokenKeywordModule, TokenIdentifier "culo", TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "module abc::def {}" [TokenKeywordModule, TokenIdentifier "abc", TokenSeparatorDColon, TokenIdentifier "def", TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "module 0xABCD::culo {}" [TokenKeywordModule, TokenLiteralIntHex "0xABCD", TokenSeparatorDColon, TokenIdentifier "culo", TokenSeparatorLBrace, TokenSeparatorRBrace]

testScanDecimal :: Spec
testScanDecimal = describe "Parse decimals" $ do
  testScan "0" [TokenLiteralIntDec 0]
  testScan "1" [TokenLiteralIntDec 1]
  testScan "123" [TokenLiteralIntDec 123]

testScanHex :: Spec
testScanHex = describe "Parse hexadecimals" $ do
  testScan "0x0" [TokenLiteralIntHex "0x0"]
  testScan "0x1" [TokenLiteralIntHex "0x1"]
  testScan "0x123" [TokenLiteralIntHex "0x123"]
  testScan "0xFF2E" [TokenLiteralIntHex "0xFF2E"]

testScanString :: Spec
testScanString = describe "Parse strings" $ do
  testScan "\"\"" [TokenLiteralString "\"\""]
  testScan "\"hello\"" [TokenLiteralString "\"hello\""]

spec :: Spec
spec = do
  testScanBraces
  testScanModule
  testScanDecimal
  testScanHex
  testScanString
