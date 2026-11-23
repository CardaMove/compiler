module Move.TranslationsMSL.PreProcessingMSLSpec (spec) where

import Control.Exception (evaluate)
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks, stripMoveFunctions)
import Test.Hspec

-- |
-- Returns the target of each spec block of the given root, in order
specBlockTargets :: Root -> [SpecBlockTarget]
specBlockTargets root =
  let topLevels = case root of
        RModule Module {moduleTopLevels} -> moduleTopLevels
        RScript Script {scriptTopLevels} -> scriptTopLevels
   in [specBlockTarget | TopLevelSpecBlock SpecBlock {specBlockTarget} <- topLevels]

-- |
-- Removes the annotations of every function spec block target, restoring the parsed form
removeAnnotations :: Root -> Root
removeAnnotations (RModule m@Module {moduleTopLevels}) = RModule m {moduleTopLevels = map removeFromTopLevel moduleTopLevels}
removeAnnotations (RScript s@Script {scriptTopLevels}) = RScript s {scriptTopLevels = map removeFromTopLevel scriptTopLevels}

-- |
-- Removes the annotations of a function spec block target, leaving any other top level unchanged
removeFromTopLevel :: TopLevel -> TopLevel
removeFromTopLevel (TopLevelSpecBlock b@SpecBlock {specBlockTarget = SpecBlockTargetFunction ident _}) =
  TopLevelSpecBlock b {specBlockTarget = SpecBlockTargetFunction ident Nothing}
removeFromTopLevel tl = tl

testAnnotateSpecBlocks :: Spec
testAnnotateSpecBlocks = describe "Tests the function `annotateSpecBlocks`" $ do
  it "Annotates each function spec block of a module with the type parameters and parameters of its target function" $ do
    moduleStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_0.move"

    let module' = parse $ scan moduleStr
        annotated = annotateSpecBlocks module'
        u64Type = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []
        tType = TypeConstructor (LocalNameAccessChain $ Identifier "T") []

    specBlockTargets annotated
      `shouldBe` [ SpecBlockTargetFunction
                     (Identifier "generic_id")
                     ( Just $
                         SpecBTFAnnotations
                           [TypeParameter False (Identifier "T") [Copy, Drop], TypeParameter False (Identifier "U") []]
                           [Parameter (Identifier "x") tType Nothing]
                     ),
                   -- Spec schemas are left untouched
                   SpecBlockTargetSchema (Identifier "AddSchema") [TypeParameter False (Identifier "T") []],
                   SpecBlockTargetFunction
                     (Identifier "add")
                     (Just $ SpecBTFAnnotations [] [Parameter (Identifier "a") u64Type Nothing, Parameter (Identifier "b") u64Type Nothing])
                 ]
    -- Nothing else changes
    removeAnnotations annotated `shouldBe` module'

  it "Annotates the function spec blocks of a script" $ do
    scriptStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_2.move"

    let script = parse $ scan scriptStr
        annotated = annotateSpecBlocks script
        signerRef = TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") []
        u64Type = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []

    specBlockTargets annotated
      `shouldBe` [ SpecBlockTargetFunction
                     (Identifier "do_nothing")
                     ( Just $
                         SpecBTFAnnotations
                           [TypeParameter False (Identifier "CoinType") []]
                           [Parameter (Identifier "s") signerRef Nothing, Parameter (Identifier "amount") u64Type Nothing]
                     )
                 ]
    removeAnnotations annotated `shouldBe` script

  it "Throws an error when the target function of a spec block is not declared in the module" $ do
    moduleStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_1.move"

    let annotated = annotateSpecBlocks $ parse $ scan moduleStr

    evaluate (length $ show annotated) `shouldThrow` errorCall "Spec block target function not found in the module: Identifier \"sub\""

testStripMoveFunctions :: Spec
testStripMoveFunctions = describe "Tests the function `stripMoveFunctions`" $ do
  it "Removes the functions of modules and scripts, and the use members importing user-defined functions" $ do
    moduleStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_3.move"
    scriptStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_4.move"
    expectedModuleStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_5.move"
    expectedScriptStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_6.move"

    let roots = map (parse . scan) [moduleStr, scriptStr]
        expectedRoots = map (parse . scan) [expectedModuleStr, expectedScriptStr]

    -- In the script: `increment` is dropped from the first use, the second use (only functions) is removed,
    -- while the module import, the schema import and `std::signer::{address_of}` (not a user module) are kept
    stripMoveFunctions roots `shouldBe` expectedRoots

  it "Keeps the spec block annotations computed before stripping the functions" $ do
    moduleStr <- readFile "test/Move/TranslationsMSL/files/PreProcessingMSLSpec_3.move"

    let stripped = stripMoveFunctions [annotateSpecBlocks $ parse $ scan moduleStr]
        signerRef = TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") []

    concatMap specBlockTargets stripped
      `shouldBe` [ SpecBlockTargetFunction (Identifier "increment") (Just $ SpecBTFAnnotations [] [Parameter (Identifier "s") signerRef Nothing]),
                   SpecBlockTargetSchema (Identifier "CounterSchema") []
                 ]

spec :: Spec
spec = do
  testAnnotateSpecBlocks
  testStripMoveFunctions
