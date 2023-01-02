{-# OPTIONS_GHC -Wno-missing-methods #-}

module Numc.AST where

data Expr
  = Val Double
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Fun Expr
  | Exe Expr Expr
  deriving Show

infixl 5 :+
infixl 5 :-
infixl 6 :*
infixl 6 :/

instance Num Expr where
  fromInteger = Val . fromInteger
