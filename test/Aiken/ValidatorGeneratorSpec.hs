module Aiken.ValidatorGeneratorSpec (spec) where

import Aiken.ValidatorGenerator (ScriptMainMetadata (..), collectScriptMainMetadata, generateValidator, scriptMainParameters)
import Data.List (isInfixOf)
import Data.Text (unpack)
import Move.AST
import Test.Hspec

spec :: Spec
spec = do
  describe "scriptMainParameters" $ do
    it "Extracts non-CPS parameters from the only function in a script" $ do
      let script = mkScript [mkFunction "do_work" [mkParam "x" (primitiveType "u64"), mkParam "cps" (primitiveType "CPS")]]
      scriptMainParameters script `shouldBe` [mkParam "x" (primitiveType "u64")]

  describe "collectScriptMainMetadata" $ do
    it "Collects metadata for transpiled scripts" $ do
      let files =
            [ ("test/Move/Loader/files/scripts/script0.move", RScript $ mkScript [mkFunction "run" [mkParam "x" (primitiveType "u64"), mkParam "cps" (primitiveType "CPS")]]),
              ("test/Move/Loader/files/scripts/script-user.move", RScript $ mkScript [mkFunction "join_round" [mkParam "sender" (TypeImmutableRef (primitiveType "signer")), mkParam "cps" (primitiveType "CPS")]])
            ]

      let metas = collectScriptMainMetadata files
      length metas `shouldBe` 2
      map scriptMainParametersMeta metas `shouldBe`
        [ [mkParam "x" (primitiveType "u64")],
          [mkParam "sender" (TypeImmutableRef (primitiveType "signer"))]
        ]

  describe "generateValidator" $ do
    it "Generates redeemer variants and spend dispatch branches" $ do
      let files =
            [ ("test/Move/Loader/files/scripts/script0.move", RScript $ mkScript [mkFunction "run" [mkParam "x" (primitiveType "u64"), mkParam "cps" (primitiveType "CPS")]]),
              ("test/Move/Loader/files/scripts/script-user.move", RScript $ mkScript [mkFunction "join_round" [mkParam "sender" (TypeImmutableRef (primitiveType "signer")), mkParam "cps" (primitiveType "CPS")]])
            ]

      let metas = collectScriptMainMetadata files
      let out = unpack $ generateValidator metas
      out `shouldSatisfy` isInfixOf "pub type Redeemer"
      out `shouldSatisfy` isInfixOf "use scripts/script0"
      out `shouldSatisfy` isInfixOf "use scripts/script-user"
      out `shouldSatisfy` isInfixOf "Script0(Int)"
      out `shouldSatisfy` isInfixOf "ScriptUser"
      out `shouldSatisfy` isInfixOf "script0.run(arg0, cps)"
      out `shouldSatisfy` isInfixOf "expect Some(arg0) = list.at(self.extra_signatories, 0)"
      out `shouldSatisfy` isInfixOf "let arg0 = Signer{address: arg0}"
      out `shouldSatisfy` isInfixOf "let cps = scope_utils.post_scope(\"arg0\", arg0, cps)"
      out `shouldSatisfy` isInfixOf "script-user.join_round(reference_utils.make_ref(\"arg0\", [], cps), cps)"

    it "Skips signer parameters in redeemer and binds extra signatories" $ do
      let files =
            [ ("test/Move/Loader/files/scripts/script-mixed.move", RScript $ mkScript [mkFunction "mixed" [mkParam "sender" (TypeImmutableRef (primitiveType "signer")), mkParam "count" (primitiveType "u64"), mkParam "cps" (primitiveType "CPS")]]),
              ("test/Move/Loader/files/scripts/script-double.move", RScript $ mkScript [mkFunction "double_signer" [mkParam "a" (primitiveType "signer"), mkParam "b" (TypeMutableRef (primitiveType "signer")), mkParam "cps" (primitiveType "CPS")]])
            ]

      let metas = collectScriptMainMetadata files
      let out = unpack $ generateValidator metas
      out `shouldSatisfy` isInfixOf "ScriptMixed(Int)"
      out `shouldSatisfy` isInfixOf "ScriptDouble"
      out `shouldSatisfy` isInfixOf "expect Some(arg0) = list.at(self.extra_signatories, 0)"
      out `shouldSatisfy` isInfixOf "expect Some(arg1) = list.at(self.extra_signatories, 1)"
      out `shouldSatisfy` isInfixOf "let arg0 = Signer{address: arg0}"
      out `shouldSatisfy` isInfixOf "let arg1 = Signer{address: arg1}"
      out `shouldSatisfy` isInfixOf "let cps = scope_utils.post_scope(\"arg1\", arg1, cps)"
      out `shouldSatisfy` isInfixOf "script-mixed.mixed(reference_utils.make_ref(\"arg0\", [], cps), arg1, cps)"
      out `shouldSatisfy` isInfixOf "script-double.double_signer(arg0, reference_utils.make_ref(\"arg1\", [], cps), cps)"

mkScript :: [TopLevel] -> Script
mkScript tops = Script {scriptTopLevels = tops}

mkFunction :: String -> [Parameter] -> TopLevel
mkFunction name params =
  TopLevelFunction
    Function
      { functionHasNativeModifier = False,
        functionVisibilityModifier = Nothing,
        functionHasEntryModifier = False,
        functionName = Identifier name,
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
