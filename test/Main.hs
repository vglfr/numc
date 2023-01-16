module Main where

import Test.Hspec (hspec)

import AST (testReadShow)
import Parser (testParseBin, testParseVal)

main :: IO ()
main = do
  hspec $ do
    testParseBin
    testParseVal
  testReadShow
