module Main where

import Test.Hspec (hspec)

import AST (testReadShow)
import Parser (testParseBin, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testParseVar
    testParseBin
    testParseVal
  testReadShow
