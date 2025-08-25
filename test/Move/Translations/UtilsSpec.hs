module Move.Translations.UtilsSpec (spec) where

import Move.AST
import Move.Translations.Utils
import Test.Hspec

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
                        [ BindedField {bindFieldIdentifier = Identifier "a", bindFieldInnerBind = Nothing},
                          BindedField {bindFieldIdentifier = Identifier "b", bindFieldInnerBind = Just $ BindIdentifier $ Identifier "b_alias"}
                        ]
                    }
              }

    getBindIdentifiers bind `shouldBe` [Identifier "a", Identifier "b_alias"]

spec :: Spec
spec = do
  testGetBindIdentifiers