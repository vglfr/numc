module Main where

import Numc.Compiler (printAST, writeBin)

main :: IO ()
main = printAST >> writeBin
