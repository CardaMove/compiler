module TranslatorSpec (spec) where

import Aiken.AST qualified as Aiken
import Move.AST qualified as Move
import Test.Hspec
import Translator

test :: String -> Move.Module -> Aiken.Module -> SpecWith ()
test descr move aiken = it descr $ translate move `shouldBe` aiken

testEmptyModule :: Spec
testEmptyModule = describe "Empty module" $ do
  test "module :: {}" (Move.Module "" "" []) (Aiken.Module [])

spec :: Spec
spec = do
  testEmptyModule
