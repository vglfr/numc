module Compiler where

import Prelude hiding (readFile)

import Data.ByteString (pack, readFile, unpack)
import Test.Hspec (Spec, describe, it, shouldBe)

import Numc.Compiler (compile, ir)
import Numc.Example (b1, val1)

testCompileBin :: Spec
testCompileBin = describe "Numc.Compiler" $ do
  let check es = trim <$> (ir . compile $ es)
      trim = pack . tail . unpack

  it "compileBin b1 1 + 2" $ do
    i <- check [b1]
    o <- readFile "test/output/b1.ll"
    i `shouldBe` o

testCompileVal :: Spec
testCompileVal = describe "Numc.Compiler" $ do
  it "compileVal val1 5" $ do
    compile [val1] `shouldBe` undefined
