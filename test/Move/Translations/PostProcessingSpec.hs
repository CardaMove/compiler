module Move.Translations.PostProcessingSpec (spec) where

import qualified Data.Map as Map
import Move.AST
import Move.Translations.PostProcessing (postProcessRoot)
import Test.Hspec

testLowercaseIdentifier :: Spec
testLowercaseIdentifier = describe "Tests the function `lowercaseIdentifier`" $ do
  it "Converts an identifier to lowercase" $ do
    let ident = Identifier "MyModule"
    -- Test via normalizeModule which uses lowercaseIdentifier internally
    let module' = Module {moduleAddress = NamedAddress (Identifier "utils"), moduleIdentifier = ident, moduleTopLevels = []}
    let root = RModule module'
    let addrMap = Map.fromList [(Identifier "utils", LiteralIntDec 1)]

    case postProcessRoot root addrMap of
      RModule result -> moduleIdentifier result `shouldBe` Identifier "mymodule"
      RScript _ -> fail "Expected RModule"

testLowercaseAddress :: Spec
testLowercaseAddress = describe "Tests the function `lowercaseAddress`" $ do
  it "Converts named addresses to lowercase" $ do
    let useDecl = Use {useAddress = NamedAddress (Identifier "MyAddr"), useIdentifier = Identifier "module", useAlias = Nothing, useMembers = []}
    let function' = Function {functionHasNativeModifier = False, functionVisibilityModifier = Nothing, functionHasEntryModifier = False, functionName = Identifier "test", functionTypeParameters = [], functionParameters = [], functionReturnType = Nothing, functionAcquires = [], functionBody = Just (Sequence {sequenceUses = [useDecl], sequenceItems = [], sequenceEndExpr = Nothing}), functionUUID = Nothing}
    let module' = Module {moduleAddress = NamedAddress (Identifier "utils"), moduleIdentifier = Identifier "test_mod", moduleTopLevels = [TopLevelFunction function']}
    let root = RModule module'
    let addrMap = Map.fromList [(Identifier "utils", LiteralIntDec 1), (Identifier "MyAddr", LiteralIntDec 2)]

    case postProcessRoot root addrMap of
      RModule resultModule -> do
        let topLevels = moduleTopLevels resultModule
        -- Find the TopLevelFunction (skip stdLibUses added by postProcessRoot)
        let functions = [f | TopLevelFunction f <- topLevels]
        case functions of
          (resultFunc:_) -> do
            case functionBody resultFunc of
              Just (Sequence {sequenceUses = (Use {useAddress = resultAddr}):_}) -> 
                resultAddr `shouldBe` NumericalAddress (LiteralIntDec 2)
              _ -> fail "Expected sequence with use declaration"
          _ -> fail "Expected to find a TopLevelFunction"
      RScript _ -> fail "Expected RModule"

  it "Converts hex addresses to lowercase" $ do
    let addrHex = NumericalAddress (LiteralIntHex "0xABCDEF")
    let useDecl = Use {useAddress = addrHex, useIdentifier = Identifier "module", useAlias = Nothing, useMembers = []}
    let function' = Function {functionHasNativeModifier = False, functionVisibilityModifier = Nothing, functionHasEntryModifier = False, functionName = Identifier "test", functionTypeParameters = [], functionParameters = [], functionReturnType = Nothing, functionAcquires = [], functionBody = Just (Sequence {sequenceUses = [useDecl], sequenceItems = [], sequenceEndExpr = Nothing}), functionUUID = Nothing}
    let module' = Module {moduleAddress = NamedAddress (Identifier "utils"), moduleIdentifier = Identifier "test_mod", moduleTopLevels = [TopLevelFunction function']}
    let root = RModule module'
    let addrMap = Map.fromList [(Identifier "utils", LiteralIntDec 1)]

    case postProcessRoot root addrMap of
      RModule resultModule -> do
        let topLevels = moduleTopLevels resultModule
        let functions = [f | TopLevelFunction f <- topLevels]
        case functions of
          (resultFunc:_) -> do
            case functionBody resultFunc of
              Just (Sequence {sequenceUses = (Use {useAddress = resultAddr}):_}) ->
                resultAddr `shouldBe` NumericalAddress (LiteralIntHex "0xabcdef")
              _ -> fail "Expected sequence with use declaration"
          _ -> fail "Expected to find a TopLevelFunction"
      RScript _ -> fail "Expected RModule"

testNormalizeModule :: Spec
testNormalizeModule = describe "Tests the function `normalizeModule`" $ do
  it "Converts module address and identifier to lowercase" $ do
    let module' = Module {moduleAddress = NamedAddress (Identifier "MyAddr"), moduleIdentifier = Identifier "MyModule", moduleTopLevels = []}
    let root = RModule module'
    let addrMap = Map.fromList [(Identifier "MyAddr", LiteralIntDec 42)]

    case postProcessRoot root addrMap of
      RModule result -> do
        moduleAddress result `shouldBe` NumericalAddress (LiteralIntDec 42)
        moduleIdentifier result `shouldBe` Identifier "mymodule"
      RScript _ -> fail "Expected RModule"

testNormalizeUse :: Spec
testNormalizeUse = describe "Tests the function `normalizeUse`" $ do
  it "Converts use address and identifier to lowercase" $ do
    let useDecl = Use {useAddress = NamedAddress (Identifier "MyAddr"), useIdentifier = Identifier "MyMod", useAlias = Nothing, useMembers = []}
    let function' = Function {functionHasNativeModifier = False, functionVisibilityModifier = Nothing, functionHasEntryModifier = False, functionName = Identifier "test", functionTypeParameters = [], functionParameters = [], functionReturnType = Nothing, functionAcquires = [], functionBody = Just (Sequence {sequenceUses = [useDecl], sequenceItems = [], sequenceEndExpr = Nothing}), functionUUID = Nothing}
    let module' = Module {moduleAddress = NamedAddress (Identifier "utils"), moduleIdentifier = Identifier "test_mod", moduleTopLevels = [TopLevelFunction function']}
    let root = RModule module'
    let addrMap = Map.fromList [(Identifier "utils", LiteralIntDec 1), (Identifier "MyAddr", LiteralIntDec 2)]

    case postProcessRoot root addrMap of
      RModule resultModule -> do
        let topLevels = moduleTopLevels resultModule
        let functions = [f | TopLevelFunction f <- topLevels]
        case functions of
          (resultFunc:_) -> do
            case functionBody resultFunc of
              Just (Sequence {sequenceUses = (Use {useAddress = resultAddr, useIdentifier = resultIdent}):_}) -> do
                resultAddr `shouldBe` NumericalAddress (LiteralIntDec 2)
                resultIdent `shouldBe` Identifier "mymod"
              _ -> fail "Expected sequence with use declaration"
          _ -> fail "Expected to find a TopLevelFunction"
      RScript _ -> fail "Expected RModule"

testPostProcessRoot :: Spec
testPostProcessRoot = describe "Tests the function `postProcessRoot`" $ do
  it "Resolves named addresses and normalizes identifiers" $ do
    let addrMap = Map.fromList [(Identifier "MyAddr", LiteralIntDec 100)]
    let module' = Module {moduleAddress = NamedAddress (Identifier "MyAddr"), moduleIdentifier = Identifier "MyModule", moduleTopLevels = []}
    let root = RModule module'

    case postProcessRoot root addrMap of
      RModule (Module {moduleAddress = addr, moduleIdentifier = ident}) -> do
        addr `shouldBe` NumericalAddress (LiteralIntDec 100)
        ident `shouldBe` Identifier "mymodule"
      RScript _ -> fail "Expected RModule"

  it "Preserves utils address unchanged" $ do
    let addrMap = Map.fromList [(Identifier "utils", LiteralIntDec 1)]
    let useDecl = Use {useAddress = NamedAddress (Identifier "utils"), useIdentifier = Identifier "MyMod", useAlias = Nothing, useMembers = []}
    let function' = Function {functionHasNativeModifier = False, functionVisibilityModifier = Nothing, functionHasEntryModifier = False, functionName = Identifier "test", functionTypeParameters = [], functionParameters = [], functionReturnType = Nothing, functionAcquires = [], functionBody = Just (Sequence {sequenceUses = [useDecl], sequenceItems = [], sequenceEndExpr = Nothing}), functionUUID = Nothing}
    let module' = Module {moduleAddress = NamedAddress (Identifier "utils"), moduleIdentifier = Identifier "test", moduleTopLevels = [TopLevelFunction function']}
    let root = RModule module'

    case postProcessRoot root addrMap of
      RModule resultModule -> do
        let topLevels = moduleTopLevels resultModule
        let functions = [f | TopLevelFunction f <- topLevels]
        case functions of
          (resultFunc:_) -> do
            case functionBody resultFunc of
              Just (Sequence {sequenceUses = (Use {useAddress = resultAddr}):_}) ->
                resultAddr `shouldBe` NamedAddress (Identifier "utils")
              _ -> fail "Expected sequence with use declaration"
          _ -> fail "Expected to find a TopLevelFunction"
      RScript _ -> fail "Expected RModule"

  it "Marks script functions as public entry" $ do
    let addrMap = Map.fromList [(Identifier "NamedAddr", LiteralIntDec 123)]
    let function' =
          Function
            { functionHasNativeModifier = False,
              functionVisibilityModifier = Nothing,
              functionHasEntryModifier = False,
              functionName = Identifier "main",
              functionTypeParameters = [],
              functionParameters = [Parameter (Identifier "x") (TypeConstructor (LocalNameAccessChain (Identifier "u64")) []) Nothing],
              functionReturnType = Nothing,
              functionAcquires = [],
              functionBody = Just (Sequence {sequenceUses = [], sequenceItems = [], sequenceEndExpr = Nothing}),
              functionUUID = Nothing
            }
    let root = RScript (Script {scriptTopLevels = [TopLevelFunction function']})

    case postProcessRoot root addrMap of
      RScript (Script {scriptTopLevels = topLevels}) -> do
        let functions = [f | TopLevelFunction f <- topLevels]
        case functions of
          (resultFunc:_) -> do
            functionVisibilityModifier resultFunc `shouldBe` Just VisibilityModifierPublic
            functionHasEntryModifier resultFunc `shouldBe` True
          _ -> fail "Expected to find a TopLevelFunction"
      RModule _ -> fail "Expected RScript"

spec :: Spec
spec = do
  testLowercaseIdentifier
  testLowercaseAddress
  testNormalizeModule
  testNormalizeUse
  testPostProcessRoot
