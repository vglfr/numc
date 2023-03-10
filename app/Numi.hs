{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Trifecta (Result (Failure, Success))

import Numc.AST (Expr)
import Numc.Compiler (compile)
import Numc.JIT (jit)
import Numc.Parser (parse)

main :: IO [Expr]
main = main' []
 where
  main' c = do
    s <- ps1 >> try @IOError getLine >>= handleEOF
    f <- case parse s of
           Success e -> pure e
           Failure e -> print e >> main' c
    let o  = compile (c <> f)
        -- c' = undefined
    jit o >>= putStrLn >> main' c
  ps1 = putStr "> " >> hFlush stdout
  handleEOF = either (const exitSuccess) pure
