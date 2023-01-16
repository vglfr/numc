{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Trifecta (Result (Failure, Success))

import Numc.AST (Expr)
import Numc.Codegen (eval)
import Numc.Parser (parseExpr)

main :: IO Expr
main = ps1 >> try @IOError getLine >>= handleEOF >>= parseLine >>= eval >>= print >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  handleEOF = either (const exitSuccess) pure
  parseLine s = case parseExpr s of
                  Success e -> pure e
                  Failure e -> print e >> main
