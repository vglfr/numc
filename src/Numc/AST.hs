module Numc.AST where

data Expr = Val Int | Bin Op Int Int deriving Show

data Op = Add | Sub | Mul | Div deriving Show

-- a :: Expr
-- a = Op Add 1 2

-- parse :: IO ()
-- parse = print a
