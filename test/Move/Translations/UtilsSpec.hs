module Move.Translations.UtilsSpec (spec) where

import Control.Monad.State (evalState, modify, runState)
import Data.Map qualified as Map
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), annotateBindingsWithUUID, extractVariablesFromSingleBind, iterateWithOthers)
import Test.Hspec

testExtractVariablesFromSingleBind :: Spec
testExtractVariablesFromSingleBind = describe "Tests the function `extractVariablesFromSingleBind`" $ do
  it "Returns all the binded identifiers given a single bind, along with their UUID and Type" $ do
    {- Code as follows:
      let MyStruct{a, b: b_alias} = e
    -}
    let scopes :: [Scope] =
          [ Map.singleton
              (LocalNameAccessChain $ Identifier "MyStruct")
              ( VariableAnnotations Nothing $
                  IntermediateTypeNamedStructDeclaration
                    []
                    [ NamedField {fieldIdentifier = Identifier "a", fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []},
                      NamedField {fieldIdentifier = Identifier "b", fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") []}
                    ]
              )
          ]

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

    extractVariablesFromSingleBind bind Nothing Nothing scopes
      `shouldBe` [ (Identifier "a", VariableAnnotations (Just 0) (TypeConstructor (LocalNameAccessChain $ Identifier "u64") [])),
                   (Identifier "b_alias", VariableAnnotations (Just 1) (TypeConstructor (LocalNameAccessChain $ Identifier "bool") []))
                 ]

testAnnotateBindingsWithUUID :: Spec
testAnnotateBindingsWithUUID = describe "Tests the function `annotateBindingsWithUUID`" $ do
  it "Assigns incremental UUIDs to all bindings, starting from zero" $ do
    fromModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_0.move"
    toModuleStr <- readFile "test/Move/Translations/files/UtilsSpec_1.txt"

    let fromModule = parse $ scan fromModuleStr
    let toModule = read toModuleStr :: Root

    -- TODO: Seems that transformBiM performs an in-order traversal
    -- Check order of UUIDs when nested code blocks
    let annotated = evalState (annotateBindingsWithUUID fromModule) 0

    annotated `shouldBe` toModule

testIterateWithOthers :: Spec
testIterateWithOthers = describe "Tests the function `iterateWithOthers`" $ do
  it "Should correctly loop over each element and provide all the others along" $ do
    let elems = ["A", "B", "C", "D", "E"]

    let (iterated, st) =
          runState
            ( iterateWithOthers
                ( \el others -> do
                    modify (+ 1)
                    return (el, others)
                )
                elems
            )
            0

    iterated `shouldBe` [("A", ["B", "C", "D", "E"]), ("B", ["A", "C", "D", "E"]), ("C", ["A", "B", "D", "E"]), ("D", ["A", "B", "C", "E"]), ("E", ["A", "B", "C", "D"])]
    st `shouldBe` length elems

spec :: Spec
spec = do
  testExtractVariablesFromSingleBind
  testAnnotateBindingsWithUUID
  testIterateWithOthers