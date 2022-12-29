module Main where

import Data.ByteString.Char8 (putStrLn)
import Prelude hiding (putStrLn)

import Numc (add, fstr, main', printf, toIR, toMod)

main :: IO ()
main = do
  s <- toIR . toMod $ [fstr, printf, add, main']
  putStrLn s
