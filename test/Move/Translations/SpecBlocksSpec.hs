module Move.Translations.SpecBlocksSpec (spec) where

import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.SpecBlocks (stripSpecBlocksInRoot)
import Test.Hspec

-- | Parses both sources and checks that stripping the spec blocks from the first gives the second
testStrip :: String -> String -> String -> Spec
testStrip description source expected = it description $ do
  stripSpecBlocksInRoot (parse (scan source)) `shouldBe` parse (scan expected)

testStripSpecBlocks :: Spec
testStripSpecBlocks = describe "Tests the function `stripSpecBlocksInRoot`" $ do
  testStrip
    "Removes the spec blocks of a module, including schemas"
    "module 0x1::M { fun f(x: u64): u64 { x } spec f { ensures result == x; } spec schema S<T> { a: u64; aborts_if a == 0; } }"
    "module 0x1::M { fun f(x: u64): u64 { x } }"

  testStrip
    "Removes the uses and the use members referenced only by the specs"
    ( unlines
        [ "script {",
          "  use std::signer::{address_of};",
          "  use 0x1::A::{Self, X, Y as Z};",
          "  use 0x1::B;",
          "  use 0x1::C as D;",
          "  fun main(s: &signer) { A::f(); let _y: Z = A::g(); }",
          "  spec main { let a = address_of(s); ensures exists<X>(a); ensures B::p() ==> D::q(); }",
          "}"
        ]
    )
    ( unlines
        [ "script {",
          "  use 0x1::A::{Self, Y as Z};",
          "  fun main(s: &signer) { A::f(); let _y: Z = A::g(); }",
          "}"
        ]
    )

  testStrip
    "Keeps the uses that nothing references, and those used by both the code and the specs"
    ( unlines
        [ "module 0x1::M {",
          "  use 0x1::Unused;",
          "  use 0x1::A::{X};",
          "  fun f(): X { A::make() }",
          "  spec f { ensures result == A::make(); ensures exists<X>(@0x1); }",
          "}"
        ]
    )
    ( unlines
        [ "module 0x1::M {",
          "  use 0x1::Unused;",
          "  use 0x1::A::{X};",
          "  fun f(): X { A::make() }",
          "}"
        ]
    )

  testStrip
    "Leaves a root without spec blocks unchanged"
    "module 0x1::M { use 0x1::A; fun f() { } }"
    "module 0x1::M { use 0x1::A; fun f() { } }"

spec :: Spec
spec = testStripSpecBlocks
