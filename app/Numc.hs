module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName)
import Text.Trifecta (Result (Failure, Success))

import Numc.Codegen (boilerplate, writeBin, writeLL)
import Numc.Compiler (compile)
import Numc.Parser (parse)

main :: IO ()
main = do
  as <- getArgs
  p <- if null as
         then die "Path to Num file needs to be provided as a first argument"
         else pure $ head as
  s <- readFile p
  f <- case parse s of
          Success e -> pure e
          Failure e -> die $ show e
  let o = boilerplate $ compile f
      b = "bin/" <> takeBaseName p
  writeLL o (b <> ".ll")
  writeBin o b
