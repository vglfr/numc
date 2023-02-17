{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Trifecta (eof, foldResult, parseString)

import Numc.AST (Expr ((:+), (:-), (:*), (:/)))
import Numc.Example (a1, b1, b2, b3, b4, val1, val2, val3, var1)
import Numc.Parser (parseAss, parseBin, parseVal, parseVar)

testParseAss :: Spec
testParseAss = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseAss <* eof) mempty

  it "parseAss a1 x = 5" $ do
    parse "x = 5" `shouldBe` Just a1

testParseBin :: Spec
testParseBin = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseBin <* eof) mempty

  it "parseBin val1 5" $ do
    parse "5" `shouldBe` Just val1

  it "parseBin var1 x" $ do
    parse "x" `shouldBe` Just var1

  it "parseBin var1 (( x ))" $ do
    parse "(( x ))" `shouldBe` Just var1

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

testParseVar :: Spec
testParseVar = describe "Numc.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVar <* eof) mempty

  it "parseVar var1 x" $ do
    parse "x" `shouldBe` Just var1

  it "parseVar var1 (x)" $ do
    parse "(x)" `shouldBe` Just var1

  it "parseVar var1 ((x))" $ do
    parse "((x))" `shouldBe` Just var1

  it "parseVar var1 ( (x) )" $ do
    parse "( (x) )" `shouldBe` Just var1

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

  it "parseVal val1 5" $ do
    parse "5" `shouldBe` Just val1

  it "parseVal val1 +5" $ do
    parse "+5" `shouldBe` Just val1

  it "parseVal val3 -5" $ do
    parse "-5" `shouldBe` Just val3

  it "parseVal \"\" (empty string)" $ do
    parse "" `shouldBe` Nothing

  it "parseVal \"-\" (minus sign only)" $ do
    parse "-" `shouldBe` Nothing

  it "parseVal \"+\" (plus sign only)" $ do
    parse "+" `shouldBe` Nothing

  it "parseVal val2 5.5" $ do
    parse "5.5" `shouldBe` Just val2

  it "parseVal val1 5." $ do
    parse "5." `shouldBe` Nothing

  it "parseVal val3 -5." $ do
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
    parse "(5)" `shouldBe` Just val1

  it "parseVal ((5))" $ do
    parse "((5))" `shouldBe` Just val1

  it "parseVal ((5)" $ do
    parse "((5)" `shouldBe` Nothing

  it "parseVal 5))" $ do
    parse "5))" `shouldBe` Nothing

  it "parseVal 5)" $ do
    parse "5)" `shouldBe` Nothing

  it "parseVal (5" $ do
    parse "(5" `shouldBe` Nothing

  it "parseVal ( 5 )" $ do
    parse "( 5 )" `shouldBe` Just val1

  it "parseVal ( ( 5 ) )" $ do
    parse "( ( 5 ) )" `shouldBe` Just val1

  it "parseVal ( (5) )" $ do
    parse "( (5) )" `shouldBe` Just val1

  it "parseVal (( 5 ))" $ do
    parse "(( 5 ))" `shouldBe` Just val1

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
