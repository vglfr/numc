module Main where

import Test.Hspec (hspec)

import AST (testReadShow)
import Compiler (testCompile)
import Parser (testParseAss, testParseBin, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testCompile
    testParseAss
    testParseBin
    testParseVar
    testParseVal
  testReadShow
