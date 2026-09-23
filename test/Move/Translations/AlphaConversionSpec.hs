module Move.Translations.AlphaConversionSpec (spec) where

import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.AlphaConversion (alphaConvert)
import Test.Hspec

-- | Checks the direct same-scope collision fixture.
testAlphaConvert :: Spec
testAlphaConvert = describe "Tests the function `alphaConvert`" $ do
  it "renames a colliding binding and its later references" $ do
    source <- readFile "test/Move/Translations/files/AlphaConversion.move"
    let converted = alphaConvert [parse (scan source)]
        expectedRoot =
          parse $
            scan
              "module NamedAddr::Module5 { fun main() { let a = 12; let b = a + 2; let a_0 = a + b; } }"
    converted `shouldBe` [expectedRoot]
  it "keeps shadowing in nested scopes independent" $ do
    let source = "module NamedAddr::Module5 { fun main() { let a = 1; { let a = 2; a }; let a = 3; a } }"
        expected = "module NamedAddr::Module5 { fun main() { let a = 1; { let a = 2; a }; let a_0 = 3; a_0 } }"
    alphaConvert [parse (scan source)] `shouldBe` [parse (scan expected)]
  it "renames duplicate identifiers in tuple bindings" $ do
    let source = "module NamedAddr::Module5 { fun main() { let (a, b) = (1, 2); let (a, c) = (3, 4); a + c } }"
        expected = "module NamedAddr::Module5 { fun main() { let (a, b) = (1, 2); let (a_0, c) = (3, 4); a_0 + c } }"
    alphaConvert [parse (scan source)] `shouldBe` [parse (scan expected)]
  it "renames identifiers in named-struct binding fields without changing labels" $ do
    let root =
          RModule
            Module
              { moduleAddress = NamedAddress (Identifier "NamedAddr"),
                moduleIdentifier = Identifier "Module5",
                moduleTopLevels =
                  [ TopLevelFunction
                      Function
                        { functionHasNativeModifier = False,
                          functionVisibilityModifier = Nothing,
                          functionHasEntryModifier = False,
                          functionName = Identifier "main",
                          functionTypeParameters = [],
                          functionParameters = [],
                          functionReturnType = Nothing,
                          functionAcquires = [],
                          functionBody =
                            Just
                              Sequence
                                { sequenceUses = [],
                                  sequenceItems =
                                    [ SequenceItemBindExpr
                                        Bindings
                                          { bindings =
                                              BindedSingle
                                                (BindNamedStruct
                                                  BindedNamedStruct
                                                    { bnsNameAccessChain = LocalNameAccessChain (Identifier "S"),
                                                      bnsTypeArgs = [],
                                                      bnsFields =
                                                        BindedFields
                                                          { hasPartialPattern = False,
                                                            bindedFields =
                                                              [ BindedField
                                                                  { bindFieldIdentifier = Identifier "x",
                                                                    bindFieldInnerBind = Nothing,
                                                                    bindedFieldUUID = Nothing
                                                                  }
                                                              ]
                                                          }
                                                    }),
                                            bindingsBindType = Nothing,
                                            bindingsBindExpr = Nothing
                                          },
                                      SequenceItemBindExpr
                                        Bindings
                                          { bindings = BindedSingle (BindIdentifier (Identifier "x") Nothing),
                                            bindingsBindType = Nothing,
                                            bindingsBindExpr = Nothing
                                          }
                                    ],
                                  sequenceEndExpr = Just (NameAccessChainExpr (LocalNameAccessChain (Identifier "x")))
                                },
                          functionUUID = Nothing
                        }
                  ]
              }
    let converted = alphaConvert [root]
    case converted of
      [RModule Module {moduleTopLevels = [TopLevelFunction Function {functionBody = Just Sequence {sequenceItems, sequenceEndExpr = Just endExpr}}]}] -> do
        sequenceItems `shouldSatisfy` any isRenamedStructField
        endExpr `shouldBe` NameAccessChainExpr (LocalNameAccessChain (Identifier "x_0"))
      _ -> expectationFailure "Unexpected alpha-converted root shape"
    where
      isRenamedStructField (SequenceItemBindExpr Bindings {bindings = BindedSingle (BindNamedStruct BindedNamedStruct {bnsFields = BindedFields {bindedFields = [BindedField {bindFieldIdentifier, bindFieldInnerBind = Just (BindIdentifier renamed _)}]}})}) =
        bindFieldIdentifier == Identifier "x" && renamed == Identifier "x"
      isRenamedStructField _ = False

spec :: Spec
spec = testAlphaConvert
