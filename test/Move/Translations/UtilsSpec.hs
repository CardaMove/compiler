module Move.Translations.UtilsSpec (spec) where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
  )
import Move.AST
import Move.Translations.Utils ( extractVariableFromSingleBind, annotateBindingsWithUUID, VariableAnnotations (VariableAnnotations) )
import Test.Hspec
import Move.Parser (parse)
import Move.Lexer (scan)

testExtractVariableFromSingleBind :: Spec
testExtractVariableFromSingleBind = describe "Tests the function `extractVariableFromSingleBind`" $ do
  it "Returns all the binded identifiers given a single bind, along with their UUID and Type" $ do
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
                        [ BindedField {bindFieldIdentifier = Identifier "a", bindFieldInnerBind = Nothing, bindedFieldUUID = Just 0},
                          BindedField {bindFieldIdentifier = Identifier "b", bindFieldInnerBind = Just $ BindIdentifier (Identifier "b_alias") (Just 1), bindedFieldUUID = Nothing}
                        ]
                    }
              }

    extractVariableFromSingleBind bind Nothing Nothing [] `shouldBe` [(Identifier "a", VariableAnnotations (Just 0) TypeUnknown), (Identifier "b_alias", VariableAnnotations (Just 1) TypeUnknown)]


testAnnotateBindingsWithUUID :: Spec
testAnnotateBindingsWithUUID = describe "Tests the function `annotateBindingsWithUUID`" $ do
  it "Assigns incremental UUIDs to all bindings, starting from zero" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_1.txt"

    let fromModule = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    let annotated = evalState (annotateBindingsWithUUID fromModule) 0

    annotated `shouldBe` toModule

spec :: Spec
spec = do
  testExtractVariableFromSingleBind
  testAnnotateBindingsWithUUID