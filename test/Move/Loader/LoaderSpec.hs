module Move.Loader.LoaderSpec (spec) where

import Data.List (sort)
import Move.AST (Identifier (Identifier), Module (moduleIdentifier), Root (RModule, RScript), Numerical (LiteralIntHex))
import Move.Loader.Loader (loadToml)
import Test.Hspec
import Data.Map qualified as Map

testLoadToml :: Spec
testLoadToml = describe "Tests the function `loadToml`" $ do
  it "Finds the Move.toml file and loads all scripts and modules in the LoaderTester project" $ do
    let tomlPath = "test/Move/Loader/files/Move.toml"

    files <- fst <$> loadToml tomlPath

    let scripts = [f | (_, RScript f) <- files]
    let modules = [f | (_, RModule f) <- files]

    length scripts `shouldBe` 1

    sort (map moduleIdentifier modules) `shouldBe` [Identifier "Module1", Identifier "Module2", Identifier "Module3", Identifier "Module4"]

  it "Throws an error when the Move.toml file does not exist" $ do
    let tomlPath = "test/Move/Loader/files/UnexistingProject/Move.toml"
    loadToml tomlPath `shouldThrow` errorCall ("Move.toml file not found at: " ++ show tomlPath)

  it "Correctly loads all the named addresses associations from the Move.toml file" $ do
    let tomlPath = "test/Move/Loader/files/Move.toml"

    addrAssoc <- snd <$> loadToml tomlPath

    addrAssoc `shouldBe` Map.fromList [(Identifier "NamedAddr", LiteralIntHex "0xCAFE"), (Identifier "SecondAddr", LiteralIntHex "0xABC123")]

spec :: Spec
spec = do
  testLoadToml