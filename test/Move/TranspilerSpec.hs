module Move.TranspilerSpec (spec) where

import Data.Text qualified as Text
import Move.Loader.Loader (loadToml)
import Move.Translations.AlphaConversion (alphaConvert)
import Move.Transpiler (offchainTranspiler, transpiler)
import Test.Hspec
import TypeScript.TypeScriptGenerator qualified as TypeScript

-- | The Bet sample project without its spec blocks (`test/BetNoSpec/`).
-- `test/Bet/` holds the spec-annotated contract, which the parser cannot load until spec blocks are
-- parsed (doc/msl.md, step 2); switch back to `test/Bet/Move.toml` then and delete `test/BetNoSpec/`
betNoSpecToml :: String
betNoSpecToml = "test/BetNoSpec/Move.toml"

-- | Runs the shared and the off-chain pipelines end to end on the Bet sample project.
-- `alphaConvert` was otherwise only tested on freshly parsed ASTs, not on the output of `postProcessRoot`
testOffchainTranspiler :: Spec
testOffchainTranspiler = describe "Tests the function `offchainTranspiler`" $ do
  it "Transpiles the spec-annotated Bet project" $
    pendingWith "spec blocks are not parsed yet (doc/msl.md, step 2): test/Bet/ does not load"

  it "Transpiles the Bet project and generates TypeScript for every file" $ do
    (sourceFiles, addrAssoc) <- loadToml betNoSpecToml
    offchain <- transpiler sourceFiles addrAssoc >>= offchainTranspiler

    map fst offchain `shouldBe` map fst sourceFiles

    tsTexts <- mapM (uncurry TypeScript.generateRoot) offchain
    -- Forces the whole generated text, so that any generator error surfaces here
    sum (map Text.length tsTexts) `shouldSatisfy` (> 0)

  it "Leaves no same-scope rebinding in the Bet project (alphaConvert is idempotent on its output)" $ do
    (sourceFiles, addrAssoc) <- loadToml betNoSpecToml
    onchain <- transpiler sourceFiles addrAssoc
    offchainRoots <- map snd <$> offchainTranspiler onchain

    -- `bet::join` rebinds `bet` in the same scope, so the off-chain pass must have renamed something
    offchainRoots `shouldNotBe` map snd onchain
    alphaConvert offchainRoots `shouldBe` offchainRoots

spec :: Spec
spec = testOffchainTranspiler
