module Compiler where

import Prelude hiding (readFile)

import Data.ByteString (pack, readFile, unpack)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import Numc.Compiler (compile, ir)
import Numc.Example
  (
    a1, a2, a3, a4
  , b1, b2, b3, b4, b5, b6, b20
  , e1, e2, e3, e4, e5
  , f1, f2, f3, f4
  , m1, m2, m3, m4, m5, m6, m7
  , v1, v2, v3
  , w1
  )

testCompile :: Spec
testCompile = describe "Numc.Compiler" $ do
  let compile' es = trim <$> (ir . compile $ es)
      trim = pack . tail . unpack

  it "compileVar a1 : w1 x = 5; x" $ do
    i <- compile' $ a1 : [w1]
    o <- readFile "test/output/w1.ll"
    i `shouldBe` o

  it "compileAss a1 x = 5" $ do
    i <- compile' [a1]
    o <- readFile "test/output/a1.ll"
    i `shouldBe` o

  it "compileAss a2 x = 5 + 1 / 4 - 3 * 2" $ do
    i <- compile' [a2]
    o <- readFile "test/output/a2.ll"
    i `shouldBe` o

  it "compileAss a3 x = 5 + y" $ do
    compile' [a3] `shouldThrow` anyException

  it "compileAss a4 x = 5 + f(y)" $ do
    compile' [a4] `shouldThrow` anyException

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

  it "compileBin b5 -1 + 2" $ do
    i <- compile' [b5]
    o <- readFile "test/output/b5.ll"
    i `shouldBe` o

  it "compileBin b6 1 - -2" $ do
    i <- compile' [b6]
    o <- readFile "test/output/b6.ll"
    i `shouldBe` o

  it "compileBin b20 1 + 2" $ do
    i <- compile' [b20]
    o <- readFile "test/output/b20.ll"
    i `shouldBe` o

  it "compileExe e1 f(5)" $ do
    i <- compile' [e1]
    o <- readFile "test/output/e1.ll"
    i `shouldBe` o

  it "compileExe e2 f(5, 3)" $ do
    i <- compile' [e2]
    o <- readFile "test/output/e2.ll"
    i `shouldBe` o

  it "compileExe e3 f()" $ do
    i <- compile' [e3]
    o <- readFile "test/output/e3.ll"
    i `shouldBe` o

  it "compileExe e4 f(5 + 3)" $ do
    i <- compile' [e4]
    o <- readFile "test/output/e4.ll"
    i `shouldBe` o

  it "compileExe e5 f(5 + y)" $ do
    i <- compile' [e5]
    o <- readFile "test/output/e5.ll"
    i `shouldBe` o

  it "compileFun f1 f(x) { (x + x) * x }" $ do
    i <- compile' [f1]
    o <- readFile "test/output/f1.ll"
    i `shouldBe` o

  it "compileFun f2 f(x, y) { (x + y) * x / y }" $ do
    i <- compile' [f2]
    o <- readFile "test/output/f2.ll"
    i `shouldBe` o

  it "compileFun f3 f(x, y) { x + 2; x / y }" $ do
    i <- compile' [f3]
    o <- readFile "test/output/f3.ll"
    i `shouldBe` o

  it "compileFun f4 f(x, y) { x + 2; x / y }" $ do
    i <- compile' [f4]
    o <- readFile "test/output/f4.ll"
    i `shouldBe` o

  it "compileMul m1 4 - 3; 1 + 2" $ do
    i <- compile' m1
    o <- readFile "test/output/m1.ll"
    i `shouldBe` o

  it "compileMul m2 x = 5; x / 2; y = x * 2; x + 1 - y" $ do
    i <- compile' m2
    o <- readFile "test/output/m2.ll"
    i `shouldBe` o

  it "compileMul m3 x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" $ do
    i <- compile' m3
    o <- readFile "test/output/m3.ll"
    i `shouldBe` o

  it "compileMul m4 x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" $ do
    i <- compile' m4
    o <- readFile "test/output/m4.ll"
    i `shouldBe` o

  it "compileMul m5 x = 5; x * 2" $ do
    i <- compile' m5
    o <- readFile "test/output/m5.ll"
    i `shouldBe` o

  it "compileMul m6 x = 5 + 3; x = x * 2; x / 5" $ do
    i <- compile' m6
    o <- readFile "test/output/m6.ll"
    i `shouldBe` o

  it "compileMul m7 f(z) { z * 2 }; x = f(4) * 2" $ do
    i <- compile' m7
    o <- readFile "test/output/m7.ll"
    i `shouldBe` o

  it "compileVal v1 5" $ do
    i <- compile' [v1]
    o <- readFile "test/output/v1.ll"
    i `shouldBe` o

  it "compileVal v2 5.5" $ do
    i <- compile' [v2]
    o <- readFile "test/output/v2.ll"
    i `shouldBe` o

  it "compileVal v3 -5" $ do
    i <- compile' [v3]
    o <- readFile "test/output/v3.ll"
    i `shouldBe` o

  it "compileVar w1 x" $ do
    compile' [w1] `shouldThrow` anyException
