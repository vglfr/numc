module Main where

import Data.ByteString.Char8 (putStrLn)
import Prelude hiding (putStrLn)

import Numc.Codegen (add, fstr, main', printf, runJIT, toIR, toMod)

main :: IO ()
main = do
  s <- toIR . toMod $ [fstr, printf, add, main']
  putStrLn s
  runJIT . toMod $ [fstr, printf, add, main']
