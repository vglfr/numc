{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Trifecta (Result (Failure, Success))

import Numc.AST (Expr)
import Numc.Codegen (eval)
import Numc.Parser (parse)

main :: IO Expr
main = ps1 >> try @IOError getLine >>= handleEOF >>= parseLine >>= eval >>= print >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  handleEOF = either (const exitSuccess) pure
  parseLine s = case parse s of
                  Success e -> pure . last $ e
                  Failure e -> print e >> main
