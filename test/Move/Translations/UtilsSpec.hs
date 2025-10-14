module Move.Translations.UtilsSpec (spec) where

import Move.AST
import Move.Translations.Utils ( getBindIdentifiers, annotateBindingsWithUUID )
import Test.Hspec
import Move.Parser (parse)
import Move.Lexer (scan)

testGetBindIdentifiers :: Spec
testGetBindIdentifiers = describe "Tests the function `getBindIdentifiers`" $ do
  it "Returns all the binded identifiers given a single bind" $ do
    {- Code as follows:
      let MyStruct{a, b: b_alias} = e
    -}
    let bind =
          BindNamedStruct $
            BindedNamedStruct
              { bnsNameAccessChain = LocalNameAccessChain $ Identifier "MyStruct",
                bnsTypeArgs = [],
                bnsFields =
                  BindedFields
                    { hasPartialPattern = False,
                      bindedFields =
                        [ BindedField {bindFieldIdentifier = Identifier "a", bindFieldInnerBind = Nothing, bindedFieldUUID = Nothing},
                          BindedField {bindFieldIdentifier = Identifier "b", bindFieldInnerBind = Just $ BindIdentifier (Identifier "b_alias") Nothing, bindedFieldUUID = Nothing}
                        ]
                    }
              }

    getBindIdentifiers bind `shouldBe` [Identifier "a", Identifier "b_alias"]


testAnnotateBindingsWithUUID :: Spec
testAnnotateBindingsWithUUID = describe "Tests the function `annotateBindingsWithUUID`" $ do
  it "Assigns incremental UUIDs to all bindings, starting from zero" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_1.txt"

    let fromModule = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    annotateBindingsWithUUID fromModule `shouldBe` toModule

spec :: Spec
spec = do
  testGetBindIdentifiers
  testAnnotateBindingsWithUUID