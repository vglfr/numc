module Main where

import Test.Hspec (hspec)

import AST (qtestExpr)
import Compiler (testCompile)
import Parser (testParseAss, testParseBin, testParseExe, testParseFun, testParseMod, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testCompile
    testParseAss
    testParseBin
    testParseExe
    testParseFun
    testParseMod
    testParseVar
    testParseVal
  qtestExpr
