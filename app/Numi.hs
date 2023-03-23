{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Trifecta (Result (Failure, Success))

import Numc.AST (Expr)
import Numc.Compiler (compile, isVoid)
import Numc.JIT (boilerplate, jit)
import Numc.Parser (parse)

import Numc.Example (a1)
-- import Numc.Codegen (writeLL)
-- import System.Process (readProcess)

main :: IO [Expr]
main = main' [a1]
 where
  main' c = do
    s <- ps1 >> try @IOError getLine >>= handleEOF
    f <- case parse s of
           Success e -> pure e
           Failure e -> print e >> main' c
    let m = boilerplate $ compile (c <> f)

    p <- jit m 2
    print p
    case p of
      Right (r, _) -> if isVoid m
                      then main' c
                      else print r >> main' c -- to c'
      Left e       -> print e >> main' c

    -- writeLL m "/tmp/temp.ll"
    -- readProcess "bat" ["/tmp/temp.ll"] mempty >>= putStrLn . ("---\n" <>) . init
    -- putStrLn "\n---"

    -- print r >> main' c
  ps1 = putStr "> " >> hFlush stdout
  handleEOF = either (const exitSuccess) pure
