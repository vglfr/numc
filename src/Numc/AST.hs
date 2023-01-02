{-# OPTIONS_GHC -Wno-missing-methods #-}

module Numc.AST where

data Expr
  = Val Double
  | Bin Bin
  | Fun Expr
  | Exe Expr Expr
  deriving Show

data Bin
  = Expr :+ Expr
  -- | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  deriving Show

instance Num Expr where
  fromInteger = Val . fromInteger

(:-) :: Expr -> Expr -> Expr
(:-) = undefined

-- a :: Expr
-- a = Op Add 1 2

-- parse :: IO ()
-- parse = print a
