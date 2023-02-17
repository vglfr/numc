module Main where

import Test.Hspec (hspec)

import AST (testReadShow)
import Parser (testParseAss, testParseBin, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testParseAss
    testParseBin
    testParseVar
    testParseVal
  testReadShow
