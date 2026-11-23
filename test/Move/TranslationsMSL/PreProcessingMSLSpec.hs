module Move.TranslationsMSL.PreProcessingMSLSpec (spec) where

import Control.Exception (evaluate)
import Move.AST
import Move.Lexer (scan)
import Move.Parser (parse)
import Move.TranslationsMSL.PreProcessingMSL (annotateSpecBlocks)
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

spec :: Spec
spec = do
  testAnnotateSpecBlocks
