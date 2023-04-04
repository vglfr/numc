{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Trifecta (eof, foldResult, parseString)

import Numc.AST (Expr ((:+), (:-), (:*), (:/)))
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
import Numc.Parser (parseAss, parseBin, parseExe, parseFun, parseMod, parseVal, parseVar)

testParseMod :: Spec
testParseMod = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseMod <* eof) mempty

  it "parseMod m1 4 - 3; 1 + 2" $ do
    parse "4 - 3; 1 + 2" `shouldBe` Just m1

  it "parseMod m2 x = 5; x / 2; y = x * 2; x + 1 - y" $ do
    parse "x = 5; x / 2; y = x * 2; x + 1 - y" `shouldBe` Just m2

  it "parseMod m3 x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" $ do
    parse "x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" `shouldBe` Just m3

  it "parseMod m4 x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" $ do
    parse "x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" `shouldBe` Just m4

  it "parseMod m5 x = 5; x * 2" $ do
    parse "x = 5; x * 2" `shouldBe` Just m5

  it "parseMod m6 x = 5 + 3; x = x * 2; x / 5" $ do
    parse "x = 5 + 3; x = x * 2; x / 5" `shouldBe` Just m6

  it "parseMod m7 f(z) { z * 2 }; x = f(4) * 2" $ do
    parse "f(z) { z * 2 }; x = f(4) * 2" `shouldBe` Just m7

testParseFun :: Spec
testParseFun = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseFun <* eof) mempty

  it "parseFun f1 f(x) { (x + x) * x }" $ do
    parse "f(x) { (x + x) * x }" `shouldBe` Just f1

  it "parseFun f2 f(x, y) { (x + y) * x / y }" $ do
    parse "f(x, y) { (x + y) * x / y }" `shouldBe` Just f2

  it "parseFun f3 f(x, y) { x + 2; x / y }" $ do
    parse "f(x, y) { x + 2; x / y }" `shouldBe` Just f3

  it "parseFun f4 f() { 5 }" $ do
    parse "f() { 5 }" `shouldBe` Just f4

testParseExe :: Spec
testParseExe = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseExe <* eof) mempty

  it "parseExe e1 f(5)" $ do
    parse "f(5)" `shouldBe` Just e1

  it "parseExe e2 f(5, 3)" $ do
    parse "f(5, 3)" `shouldBe` Just e2

  it "parseExe e3 f()" $ do
    parse "f()" `shouldBe` Just e3

  it "parseExe e4 f(5 + 3)" $ do
    parse "f(5 + 3)" `shouldBe` Just e4

  it "parseExe e5 f(5 + y)" $ do
    parse "f(5 + y)" `shouldBe` Just e5

testParseAss :: Spec
testParseAss = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseAss <* eof) mempty

  it "parseAss a1 x = 5" $ do
    parse "x = 5" `shouldBe` Just a1

  it "parseAss a2 x = 5 + 1 / 4 - 3 * 2" $ do
    parse "x = 5 + 1 / 4 - 3 * 2" `shouldBe` Just a2

  it "parseAss a3 x = 5 + y" $ do
    parse "x = 5 + y" `shouldBe` Just a3

  it "parseAss a4 x = 5 + f(y)" $ do
    parse "x = 5 + f(y)" `shouldBe` Just a4

testParseBin :: Spec
testParseBin = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseBin <* eof) mempty

  it "parseBin v1 5" $ do
    parse "5" `shouldBe` Just v1

  it "parseBin v1 x" $ do
    parse "x" `shouldBe` Just w1

  it "parseBin v1 (( x ))" $ do
    parse "(( x ))" `shouldBe` Just w1

  it "parseBin ((1 + 2))" $ do
    parse "((1 + 2))" `shouldBe` Just b1

  it "parseBin (((1 + 2)))" $ do
    parse "(((1 + 2)))" `shouldBe` Just b1

  it "parseBin ( ( ( 1 + 2 ) ) )" $ do
    parse "( ( ( 1 + 2 ) ) )" `shouldBe` Just b1

  it "parseBin (1 + 2) " $ do
    parse "(1 + 2) " `shouldBe` Just b1

  it "parseBin (1 + 2 ) " $ do
    parse "(1 + 2 ) " `shouldBe` Just b1

  it "parseBin (1 + 2) + (3 + 4)" $ do
    parse "(1 + 2) + (3 + 4)" `shouldBe` Just ((1 :+ 2) :+ (3 :+ 4))

  it "parseBin (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseBin (1 + (2 + 3)) + 4" $ do
    parse "(1 + (2 + 3)) + 4" `shouldBe` Just ((1 :+ (2 :+ 3)) :+ 4)

  it "parseBin (1 + (2 + 3)) + ((4))" $ do
    parse "(1 + (2 + 3)) + ((4))" `shouldBe` Just ((1 :+ (2 :+ 3)) :+ 4)

  it "parseBin ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseBin (((1 + 2)) + 3)" $ do
    parse "(((1 + 2)) + 3)" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseBin (((1 + 2) + 3))" $ do
    parse "(((1 + 2) + 3))" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseBin 1 + 2 + 3" $ do
    parse "1 + 2 + 3" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin 1 + 2 + x" $ do
    parse "1 + 2 + x" `shouldBe` Just (1 :+ 2 :+ "x")

  it "parseBin 1 + 2 - 3" $ do
    parse "1 + 2 - 3" `shouldBe` Just (1 :+ 2 :- 3)

  it "parseBin 1 * 2 - 3" $ do
    parse "1 * 2 - 3" `shouldBe` Just (1 :* 2 :- 3)

  it "parseBin 1 * 2 / 3" $ do
    parse "1 * 2 / 3" `shouldBe` Just (1 :* 2 :/ 3)

  it "parseBin (1 + 2 + 3)" $ do
    parse "(1 + 2 + 3)" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin ((1 + 2 + 3))" $ do
    parse "((1 + 2 + 3))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin (((1 + 2 + 3)))" $ do
    parse "(((1 + 2 + 3)))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin 1 + (2 + 3)" $ do
    parse "1 + (2 + 3)" `shouldBe` Just (1 :+ (2 :+ 3))

  it "parseBin ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin (1 + (2 + 3))" $ do
    parse "(1 + (2 + 3))" `shouldBe` Just (1 :+ (2 :+ 3))

  it "parseBin ((((1 + 2)) + 3))" $ do
    parse "((((1 + 2)) + 3))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseBin ((1 + ((2 + 3))))" $ do
    parse "((1 + ((2 + 3))))" `shouldBe` Just (1 :+ (2 :+ 3))

  it "parseBin 1 + 2" $ do
    parse "1 + 2" `shouldBe` Just b1

  it "parseBin 1 - 2" $ do
    parse "1 - 2" `shouldBe` Just b2

  it "parseBin 1 * 2" $ do
    parse "1 * 2" `shouldBe` Just b3

  it "parseBin 1 / 2" $ do
    parse "1 / 2" `shouldBe` Just b4

  it "parseBin (1 + 2)" $ do
    parse "(1 + 2)" `shouldBe` Just b1

  it "parseBin ( 1 + 2 )" $ do
    parse "( 1 + 2 )" `shouldBe` Just b1

  it "parseBin ((1 + 2))" $ do
    parse "((1 + 2))" `shouldBe` Just b1

  it "parseBin (((1 + 2)))" $ do
    parse "(((1 + 2)))" `shouldBe` Just b1

  it "parseBin (1+2)" $ do
    parse "(1+2)" `shouldBe` Just b1

  it "parseBin ((1) + (2))" $ do
    parse "((1) + (2))" `shouldBe` Just b1

  it "parseBin (((1) + (2)))" $ do
    parse "(((1) + (2)))" `shouldBe` Just b1

  it "parseBin (((1)+(2)))" $ do
    parse "(((1)+(2)))" `shouldBe` Just b1

  it "parseBin ((((1)) + ((2))))" $ do
    parse "((((1)) + ((2))))" `shouldBe` Just b1

  it "parseBin (( (( 1 )) + (( 2 )) ))" $ do
    parse "(( (( 1 )) + (( 2 )) ))" `shouldBe` Just b1

  it "parseBin ( ( ( ( 1 ) ) + ( ( 2 ) ) ) )" $ do
    parse "( ( ( ( 1 ) ) + ( ( 2 ) ) ) )" `shouldBe` Just b1

  it "parseBin ((1)) + ((2))" $ do
    parse "((1)) + ((2))" `shouldBe` Just b1

  it "parseBin (((1)) + ((2)))" $ do
    parse "(((1)) + ((2)))" `shouldBe` Just b1

  it "parseBin (1) + (2)" $ do
    parse "(1) + (2)" `shouldBe` Just b1

  it "parseBin ((1) + 2)" $ do
    parse "((1) + 2)" `shouldBe` Just b1

  it "parseBin (((1)) + 2)" $ do
    parse "(((1)) + 2)" `shouldBe` Just b1

  it "parseBin 1 + 2)" $ do
    parse "1 + 2)" `shouldBe` Nothing

  it "parseBin (1 + 2" $ do
    parse "(1 + 2" `shouldBe` Nothing

  it "parseBin -1 + 2" $ do
    parse "-1 + 2" `shouldBe` Just b5

  it "parseBin 1 - -2" $ do
    parse "1 - -2" `shouldBe` Just b6

  it "parseBin b20 5 + 1 - 3 + 6 * 2 / 4" $ do
    parse "5 + 1 - 3 + 6 * 2 / 4" `shouldBe` Just b20

testParseVar :: Spec
testParseVar = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVar <* eof) mempty

  it "parseVar w1 x" $ do
    parse "x" `shouldBe` Just w1

  it "parseVar w1 (x)" $ do
    parse "(x)" `shouldBe` Just w1

  it "parseVar w1 ((x))" $ do
    parse "((x))" `shouldBe` Just w1

  it "parseVar w1 ( (x) )" $ do
    parse "( (x) )" `shouldBe` Just w1

  it "parseVar x1_X" $ do
    parse "x1_X" `shouldBe` Just "x1_X"

  it "parseVar x'" $ do
    parse "x'" `shouldBe` Just "x'"

  it "parseVar x'x" $ do
    parse "x'x" `shouldBe` Just "x'x"

  it "parseVar _x" $ do
    parse "_x" `shouldBe` Nothing

  it "parseVar X" $ do
    parse "X" `shouldBe` Nothing

  it "parseVar 'x" $ do
    parse "'x" `shouldBe` Nothing

  it "parseVar 1x" $ do
    parse "1x" `shouldBe` Nothing

  it "parseVar 1" $ do
    parse "1" `shouldBe` Nothing

  it "parseVar _" $ do
    parse "_" `shouldBe` Nothing

  it "parseVar '" $ do
    parse "'" `shouldBe` Nothing

  it "parseVar x+x" $ do
    parse "x+x" `shouldBe` Nothing

testParseVal :: Spec
testParseVal = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVal <* eof) mempty

  it "parseVal v1 5" $ do
    parse "5" `shouldBe` Just v1

  it "parseVal v1 +5" $ do
    parse "+5" `shouldBe` Just v1

  it "parseVal v3 -5" $ do
    parse "-5" `shouldBe` Just v3

  it "parseVal \"\" (empty string)" $ do
    parse "" `shouldBe` Nothing

  it "parseVal \"-\" (minus sign only)" $ do
    parse "-" `shouldBe` Nothing

  it "parseVal \"+\" (plus sign only)" $ do
    parse "+" `shouldBe` Nothing

  it "parseVal v2 5.5" $ do
    parse "5.5" `shouldBe` Just v2

  it "parseVal v1 5." $ do
    parse "5." `shouldBe` Nothing

  it "parseVal v3 -5." $ do
    parse "-5." `shouldBe` Nothing

  it "parseVal .5" $ do
    parse ".5" `shouldBe` Nothing

  it "parseVal -.5" $ do
    parse "-.5" `shouldBe` Nothing

  it "parseVal 5.5e2" $ do
    parse "5.5e2" `shouldBe` Just 550

  it "parseVal 5.5E2" $ do
    parse "5.5E2" `shouldBe` Just 550

  it "parseVal 5.5e+2" $ do
    parse "5.5e+2" `shouldBe` Just 550

  it "parseVal 5.5E+2" $ do
    parse "5.5E+2" `shouldBe` Just 550

  it "parseVal 5.5e-2" $ do
    parse "5.5e-2" `shouldBe` Just 0.055

  it "parseVal 5.5E-2" $ do
    parse "5.5E-2" `shouldBe` Just 0.055

  it "parseVal (5)" $ do
    parse "(5)" `shouldBe` Just v1

  it "parseVal ((5))" $ do
    parse "((5))" `shouldBe` Just v1

  it "parseVal ((5)" $ do
    parse "((5)" `shouldBe` Nothing

  it "parseVal 5))" $ do
    parse "5))" `shouldBe` Nothing

  it "parseVal 5)" $ do
    parse "5)" `shouldBe` Nothing

  it "parseVal (5" $ do
    parse "(5" `shouldBe` Nothing

  it "parseVal ( 5 )" $ do
    parse "( 5 )" `shouldBe` Just v1

  it "parseVal ( ( 5 ) )" $ do
    parse "( ( 5 ) )" `shouldBe` Just v1

  it "parseVal ( (5) )" $ do
    parse "( (5) )" `shouldBe` Just v1

  it "parseVal (( 5 ))" $ do
    parse "(( 5 ))" `shouldBe` Just v1

  it "parseVal ()" $ do
    parse "()" `shouldBe` Nothing

  it "parseVal (  )" $ do
    parse "(  )" `shouldBe` Nothing

  it "parseVal ." $ do
    parse "." `shouldBe` Nothing

  it "parseVal -." $ do
    parse "-." `shouldBe` Nothing

  it "parseVal .e5" $ do
    parse ".e5" `shouldBe` Nothing

  it "parseVal (5 + 5)" $ do
    parse "(5 + 5)" `shouldBe` Nothing
