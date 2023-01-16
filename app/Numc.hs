module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Text.Trifecta (Result (Failure, Success))

-- import Numc.Compiler (printAST, writeBin)
import Numc.Parser (parseExpr)

main :: IO ()
main = readPath >>= readFile >>= parse >>= print
 where
  readPath = do
    as <- getArgs
    if null as
      then die "Path to Num file needs to be provided as a first argument"
      else pure $ head as
  parse s = case parseExpr s of
                  Success e -> pure e
                  Failure e -> die $ show e
-- main = printAST >> writeBin
