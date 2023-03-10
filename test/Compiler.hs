module Compiler where

import Prelude hiding (readFile)

import Data.ByteString (pack, readFile, unpack)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import Numc.Compiler (compile, ir)
import Numc.Example (a1, a2, b1, b2, b3, b4, m1, m2, m3, m4, m5, m6, b20, val1, val2, val3, var1)

testCompile :: Spec
testCompile = describe "Numc.Compiler" $ do
  let compile' es = trim <$> (ir . compile $ es)
      trim = pack . tail . unpack

  it "compileVal val1 5" $ do
    i <- compile' [val1]
    o <- readFile "test/output/val1.ll"
    i `shouldBe` o

  it "compileVal val2 5.5" $ do
    i <- compile' [val2]
    o <- readFile "test/output/val2.ll"
    i `shouldBe` o

  it "compileVal val3 -5" $ do
    i <- compile' [val3]
    o <- readFile "test/output/val3.ll"
    i `shouldBe` o

  it "compileBin b1 1 + 2" $ do
    i <- compile' [b1]
    o <- readFile "test/output/b1.ll"
    i `shouldBe` o

  it "compileBin b2 1 - 2" $ do
    i <- compile' [b2]
    o <- readFile "test/output/b2.ll"
    i `shouldBe` o

  it "compileBin b3 1 + 2" $ do
    i <- compile' [b3]
    o <- readFile "test/output/b3.ll"
    i `shouldBe` o

  it "compileBin b4 1 + 2" $ do
    i <- compile' [b4]
    o <- readFile "test/output/b4.ll"
    i `shouldBe` o

  it "compileBin b20 1 + 2" $ do
    i <- compile' [b20]
    o <- readFile "test/output/b20.ll"
    i `shouldBe` o

  it "compileVar a1 : [var1] x = 5; x" $ do
    i <- compile' $ a1 : [var1]
    o <- readFile "test/output/var1.ll"
    i `shouldBe` o

  it "compileVar [var1] x = 5; x" $ do
    compile' [var1] `shouldThrow` anyException

  it "compileAss a1 x = 5" $ do
    i <- compile' [a1]
    o <- readFile "test/output/a1.ll"
    i `shouldBe` o

  it "compileAss a2 x = 5 + 1 / 4 - 3 * 2" $ do
    i <- compile' [a2]
    o <- readFile "test/output/a2.ll"
    i `shouldBe` o

  it "compileMultiline m1 4 - 3; 1 + 2" $ do
    i <- compile' m1
    o <- readFile "test/output/m1.ll"
    i `shouldBe` o

  it "compileMultiline m2 x = 5; x / 2; y = x * 2; x + 1 - y" $ do
    i <- compile' m2
    o <- readFile "test/output/m2.ll"
    i `shouldBe` o

  it "compileMultiline m3 x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" $ do
    i <- compile' m3
    o <- readFile "test/output/m3.ll"
    i `shouldBe` o

  it "compileMultiline m4 x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" $ do
    i <- compile' m4
    o <- readFile "test/output/m4.ll"
    i `shouldBe` o

  it "compileMultiline m5 x = 5; x * 2" $ do
    i <- compile' m5
    o <- readFile "test/output/m5.ll"
    i `shouldBe` o

  it "compileMultiline m6 x = 5 + 3; x = x * 2; x / 5" $ do
    i <- compile' m6
    o <- readFile "test/output/m6.ll"
    i `shouldBe` o
