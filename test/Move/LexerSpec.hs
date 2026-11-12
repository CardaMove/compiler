module Move.LexerSpec (spec) where

import Move.Lexer
import Move.Token
import Test.Hspec

testScan :: String -> [Token] -> SpecWith ()
testScan str toks = it str $ scan str `shouldBe` toks

testScanBraces :: Spec
testScanBraces = describe "Scan {}" $ do
  testScan "{}" [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "{ }" [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "{ } " [TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan " { }" [TokenSeparatorLBrace, TokenSeparatorRBrace]

testScanModule :: Spec
testScanModule = describe "Scan module" $ do
  testScan "module" [TokenKeywordModule]
  testScan "module " [TokenKeywordModule]
  testScan "module {" [TokenKeywordModule, TokenSeparatorLBrace]
  testScan "module CAFFE {}" [TokenKeywordModule, TokenIdentifier "CAFFE", TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "module abc::def {}" [TokenKeywordModule, TokenIdentifier "abc", TokenSeparatorDoubleColon, TokenIdentifier "def", TokenSeparatorLBrace, TokenSeparatorRBrace]
  testScan "module 0xABCD::CAFFE {}" [TokenKeywordModule, TokenLiteralIntHex "0xABCD", TokenSeparatorDoubleColon, TokenIdentifier "CAFFE", TokenSeparatorLBrace, TokenSeparatorRBrace]

testScanDecimal :: Spec
testScanDecimal = describe "Scan decimals" $ do
  testScan "0" [TokenLiteralIntDec 0]
  testScan "1" [TokenLiteralIntDec 1]
  testScan "123" [TokenLiteralIntDec 123]
  testScan "123_456u64" [TokenLiteralIntDec 123456]
  testScan "123_456_789_u256" [TokenLiteralIntDec 123456789]
  testScan "123_456_789_" [TokenLiteralIntDec 123456789]

testScanHex :: Spec
testScanHex = describe "Scan hexadecimals" $ do
  testScan "0x0" [TokenLiteralIntHex "0x0"]
  testScan "0x1" [TokenLiteralIntHex "0x1"]
  testScan "0x123" [TokenLiteralIntHex "0x123"]
  testScan "0xFF2E" [TokenLiteralIntHex "0xFF2E"]
  testScan "0xFF_2Eu256" [TokenLiteralIntHex "0xFF2E"]
  testScan "0xFF_2E_u256" [TokenLiteralIntHex "0xFF2E"]
  testScan "0xFF_2E_" [TokenLiteralIntHex "0xFF2E"]

testScanString :: Spec
testScanString = describe "Scan strings" $ do
  testScan "\"\"" [TokenLiteralString "\"\""]
  testScan "\"hello\"" [TokenLiteralString "\"hello\""]
  -- Strings can have special characters
  testScan "\"123_string0xABC#?\\+\"" [TokenLiteralString "\"123_string0xABC#?\\+\""]

testScanBool :: Spec
testScanBool = describe "Scan booleans" $ do
  testScan "true" [TokenLiteralBool True]
  testScan "false" [TokenLiteralBool False]

testScanBinding :: Spec
testScanBinding = describe "Scan bindings" $ do
  testScan "let answer = 42" [TokenKeywordLet, TokenIdentifier "answer", TokenOperatorAssign, TokenLiteralIntDec 42]

testScanIdentifier :: Spec
testScanIdentifier = describe "Scan identifiers" $ do
  testScan "answer" [TokenIdentifier "answer"]
  testScan "camelCase" [TokenIdentifier "camelCase"]
  testScan "PascalCase" [TokenIdentifier "PascalCase"]
  testScan "snake_case" [TokenIdentifier "snake_case"]
  testScan "myAnswer0" [TokenIdentifier "myAnswer0"]
  testScan "letIdentifier" [TokenIdentifier "letIdentifier"]
  testScan "trueVariable" [TokenIdentifier "trueVariable"]
  testScan "_myVaria_ble" [TokenIdentifier "_myVaria_ble"]
  testScan "_" [TokenIdentifier "_"]

testScanOperators :: Spec
testScanOperators = describe "Scan operators" $ do
  testScan "&mut" [TokenOperatorAmpMut]
  testScan ">" [TokenOperatorGt]
  -- See Parser.y about '>>' operator
  testScan ">>" [TokenOperatorGt,TokenOperatorGt]

-- | Scans the `.move` fixture `test/Move/files/LexerSpec_<n>.move` and compares the result with
-- the expected tokens in `test/Move/files/LexerSpec_<n + 1>.txt` (printed with `show`, loaded with `read`)
testScanFile :: String -> Int -> SpecWith ()
testScanFile description n = it description $ do
  fromStr <- readFile $ "test/Move/files/LexerSpec_" ++ show n ++ ".move"
  toStr <- readFile $ "test/Move/files/LexerSpec_" ++ show (n + 1) ++ ".txt"
  scan fromStr `shouldBe` (read toStr :: [Token])

-- Keywords and operators of the Move Specification Language (MSL).
-- Fixtures are copied from rosetta-redo: `move/scripts/auction_bid.move` and `move/sources/coin_proxy/coin_proxy.move`
testScanSpecBlocks :: Spec
testScanSpecBlocks = describe "Scan MSL spec blocks" $ do
  testScanFile "Auction auction_bid: spec, let, aborts_if, include, ensures, modifies, ==>" 0
  testScanFile "CoinProxy module: doc comments, pragma, let post, spec schema" 2

spec :: Spec
spec = do
  testScanBraces
  testScanModule
  testScanDecimal
  testScanHex
  testScanString
  testScanBool
  testScanBinding
  testScanIdentifier
  testScanOperators
  testScanSpecBlocks
