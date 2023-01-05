module Main where

import Data.Either (isLeft)
import Text.Parsec (eof, parse)
import Test.Hspec (Spec, describe, it, hspec, shouldBe, shouldSatisfy)

import Numc.Example (v1, v2, v3)
import Numc.Parser (parseVal)

main :: IO ()
main = hspec testParseVal

testParseVal :: Spec
testParseVal = describe "Numc.Parser - Val" $ do
  let parseVal' = parse (parseVal <* eof) "Error parsing value"

  it "parseVal v1 (5)" $ do
    parseVal' "5" `shouldBe` Right v1

  it "parseVal v1 (+5)" $ do
    parseVal' "+5" `shouldBe` Right v1

  it "parseVal v3 (-5)" $ do
    parseVal' "-5" `shouldBe` Right v3

  it "parseVal \"\"  (empty string)" $ do
    parseVal' "" `shouldSatisfy` isLeft

  it "parseVal \"-\" (minus sign only)" $ do
    parseVal' "-" `shouldSatisfy` isLeft

  it "parseVal \"+\" (plus sign only)" $ do
    parseVal' "+" `shouldSatisfy` isLeft

  it "parseVal v2 (5.5)" $ do
    parseVal' "5.5" `shouldBe` Right v2

  it "parseVal 5." $ do
    parseVal' "5." `shouldBe` Right 5

  it "parseVal .5" $ do
    parseVal' ".5" `shouldBe` Right 0.5

  it "parseVal -5." $ do
    parseVal' "-5." `shouldBe` Right (-5)

  it "parseVal -.5" $ do
    parseVal' "-.5" `shouldBe` Right (-0.5)

  it "parseVal 5.5e2" $ do
    parseVal' "5.5e2" `shouldBe` Right 550

  it "parseVal 5.5E2" $ do
    parseVal' "5.5E2" `shouldBe` Right 550

  it "parseVal 5.5e+2" $ do
    parseVal' "5.5e+2" `shouldBe` Right 550

  it "parseVal 5.5E+2" $ do
    parseVal' "5.5E+2" `shouldBe` Right 550

  it "parseVal 5.5e-2" $ do
    parseVal' "5.5e-2" `shouldBe` Right 0.055

  it "parseVal 5.5E-2" $ do
    parseVal' "5.5E-2" `shouldBe` Right 0.055

  it "parseVal ." $ do
    parseVal' "." `shouldSatisfy` isLeft

  it "parseVal -." $ do
    parseVal' "-." `shouldSatisfy` isLeft

  it "parseVal .e5" $ do
    parseVal' ".e5" `shouldSatisfy` isLeft
