{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.AST where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString, fromString)

data Expr
  = Var String
  | Val Double
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr := Expr
  | Fun (NonEmpty Expr) Expr
  | Exe Expr (NonEmpty Expr)
  deriving Eq

infixl 5 :+
infixl 5 :-
infixl 6 :*
infixl 6 :/
infixl 1 :=

instance Show Expr where
  showsPrec n e = case e of
                    Var v -> shows v
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
                    x :+ y -> showParen (n > 5) $ showsPrec 5 x . showString " + " . showsPrec 6 y
                    x :- y -> showParen (n > 5) $ showsPrec 5 x . showString " - " . showsPrec 6 y
                    x :* y -> showParen (n > 6) $ showsPrec 6 x . showString " * " . showsPrec 7 y
                    x :/ y -> showParen (n > 6) $ showsPrec 6 x . showString " / " . showsPrec 7 y
                    x := y -> showParen (n > 1) $ showsPrec 1 x . showString " = " . showsPrec 2 y

instance Num Expr where
  fromInteger = Val . fromInteger
  negate (Val a) = Val (negate a)

instance Fractional Expr where
  fromRational = Val . fromRational

instance IsString Expr where
  fromString = Var

isVal :: Expr -> Bool
isVal (Val _) = True
isVal      _  = False

val :: Expr -> Double
val (Val x) = x
