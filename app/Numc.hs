module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName)
import Text.Trifecta (Result (Failure, Success))

import Numc.Compiler (toLL, writeBin, writeLL)
import Numc.Parser (parseExpr)

main :: IO ()
main = do
  as <- getArgs
  p <- if null as
         then die "Path to Num file needs to be provided as a first argument"
         else pure $ head as
  s <- readFile p
  e <- case parseExpr s of
         Success e -> pure e
         Failure e -> die $ show e
  let o = toLL e
      f = takeBaseName p
  writeLL o (f <> ".ll")
  writeBin o f
