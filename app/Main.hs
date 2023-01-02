{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import Numc.AST (Expr)
import Numc.Codegen (printAST, evalRepl)
-- import Numc.Codegen (add, fstr, main', printf, runJIT, toIR, toMod)

main :: IO Expr
main = ps1 >> try @IOError getLine >>= handleEOF >>= parseLine >> printAST >> evalRepl >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  handleEOF = either (const exitSuccess) pure
  parseLine _ = pure ()

-- main :: IO ()
-- main = do
--   s <- toIR . toMod $ [fstr, printf, add, main']
--   putStrLn s
--   runJIT . toMod $ [fstr, printf, add, main']
