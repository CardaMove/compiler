module Aiken.ValidatorGeneratorSpec (spec) where

import Aiken.ValidatorGenerator (collectScriptMainMetadata, generateValidator, scriptMainParameters)
import Data.List (isInfixOf)
import Data.Text (unpack)
import Move.AST
import Test.Hspec

spec :: Spec
spec = do
  describe "scriptMainParameters" $ do
    it "Extracts main function parameters from a script" $ do
      let script = mkScript [mkMain [mkParam "x" (primitiveType "u64")]]
      scriptMainParameters script `shouldBe` [mkParam "x" (primitiveType "u64")]

  describe "collectScriptMainMetadata" $ do
    it "Collects metadata for transpiled scripts" $ do
      let files =
            [ ("test/Move/Loader/files/scripts/script0.move", RScript $ mkScript [mkMain [mkParam "x" (primitiveType "u64")]]),
              ("test/Move/Loader/files/scripts/script-user.move", RScript $ mkScript [mkMain [mkParam "sender" (TypeImmutableRef (primitiveType "signer"))]])
            ]

      let metas = collectScriptMainMetadata files
      length metas `shouldBe` 2

  describe "generateValidator" $ do
    it "Generates redeemer variants and spend dispatch branches" $ do
      let files =
            [ ("test/Move/Loader/files/scripts/script0.move", RScript $ mkScript [mkMain [mkParam "x" (primitiveType "u64")]]),
              ("test/Move/Loader/files/scripts/script-user.move", RScript $ mkScript [mkMain [mkParam "sender" (TypeImmutableRef (primitiveType "signer"))]])
            ]

      let metas = collectScriptMainMetadata files
      let out = unpack $ generateValidator metas
      out `shouldSatisfy` isInfixOf "pub type Redeemer"
      out `shouldSatisfy` isInfixOf "use scripts/script0"
      out `shouldSatisfy` isInfixOf "use scripts/script-user"
      out `shouldSatisfy` isInfixOf "Script0(Int)"
      out `shouldSatisfy` isInfixOf "ScriptUser(Reference<Signer>)"
      out `shouldSatisfy` isInfixOf "script0.main(arg0)"
      out `shouldSatisfy` isInfixOf "script-user.main(arg0)"

mkScript :: [TopLevel] -> Script
mkScript tops = Script {scriptTopLevels = tops}

mkMain :: [Parameter] -> TopLevel
mkMain params =
  TopLevelFunction
    Function
      { functionHasNativeModifier = False,
        functionVisibilityModifier = Nothing,
        functionHasEntryModifier = False,
        functionName = Identifier "main",
        functionTypeParameters = [],
        functionParameters = params,
        functionReturnType = Nothing,
        functionAcquires = [],
        functionBody = Nothing,
        functionUUID = Nothing
      }

mkParam :: String -> Type -> Parameter
mkParam name t =
  Parameter
    { parameterIdentifier = Identifier name,
      parameterType = t,
      parameterUUID = Nothing
    }

primitiveType :: String -> Type
primitiveType ident = TypeConstructor (LocalNameAccessChain (Identifier ident)) []
