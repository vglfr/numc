module Main where

import Test.Hspec (hspec)

import AST (testReadShow)
import Compiler (testCompileBin, testCompileVal)
import Parser (testParseAss, testParseBin, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testCompileBin
    testCompileVal
    testParseAss
    testParseBin
    testParseVar
    testParseVal
  testReadShow
