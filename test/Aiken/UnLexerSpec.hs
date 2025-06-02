module Aiken.UnLexerSpec (spec) where

import Test.Hspec

testScan :: String -> String -> SpecWith ()
testScan a b = it a $ a`shouldBe` b

testDummy :: Spec
testDummy = describe "Dummy" $ do
  testScan "dummy" "dummy"

spec :: Spec
spec = do
  testDummy
