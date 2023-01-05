module Main where

import Test.Hspec (Spec, describe, it, hspec, shouldBe)
import Text.Trifecta (eof, foldResult, parseString)

-- import Numc.AST (Expr ((:+)))
import Numc.Example (b1, b2, b3, b4, v1, v2, v3)
import Numc.Parser (parseBin, parseExpr, parseVal)

main :: IO ()
main = hspec $ do
  testParseExpr
  testParseBin
  testParseVal

testParseExpr :: Spec
testParseExpr = describe "Numc.Parser - Expr" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseExpr <* eof) mempty

  it "parseExpr v1 (5)" $ do
    parse "5" `shouldBe` Just v1

  it "parseExpr 1 + 2" $ do
    parse "1 + 2" `shouldBe` Just b1

  -- it "parseExpr (1 + 2) + (3 + 4)" $ do
  --   parse "(1 + 2) + (3 + 4)" `shouldBe` Just ((1 :+ 2) :+ (3 :+ 4))

  -- it "parseExpr 1 + 2 + 3" $ do
  --   parse "1 + 2 + 3" `shouldBe` Just (1 :+ 2 :+ 3)

testParseBin :: Spec
testParseBin = describe "Numc.Parser - Bin" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseBin <* eof) mempty

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

testParseVal :: Spec
testParseVal = describe "Numc.Parser - Val" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVal <* eof) mempty

  it "parseVal v1 (5)" $ do
    parse "5" `shouldBe` Just v1

  it "parseVal v1 (+5)" $ do
    parse "+5" `shouldBe` Just v1

  it "parseVal v3 (-5)" $ do
    parse "-5" `shouldBe` Just v3

  it "parseVal \"\"  (empty string)" $ do
    parse "" `shouldBe` Nothing

  it "parseVal \"-\" (minus sign only)" $ do
    parse "-" `shouldBe` Nothing

  it "parseVal \"+\" (plus sign only)" $ do
    parse "+" `shouldBe` Nothing

  it "parseVal v2 (5.5)" $ do
    parse "5.5" `shouldBe` Just v2

  it "parseVal v1 (5.)" $ do
    parse "5." `shouldBe` Nothing

  it "parseVal v3 (-5.)" $ do
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
