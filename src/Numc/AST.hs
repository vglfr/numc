{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.AST where

data Expr
  = Val Double
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Fun Expr
  | Exe Expr Expr
  deriving Eq

infixl 5 :+
infixl 5 :-
infixl 6 :*
infixl 6 :/

instance Show Expr where
  showsPrec n e = case e of
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
                    x :+ y -> showParen (n > 5) $ showsPrec 5 x . showString " + " . showsPrec 6 y
                    x :- y -> showParen (n > 5) $ showsPrec 5 x . showString " - " . showsPrec 6 y
                    x :* y -> showParen (n > 6) $ showsPrec 6 x . showString " * " . showsPrec 7 y
                    x :/ y -> showParen (n > 6) $ showsPrec 6 x . showString " / " . showsPrec 7 y

instance Num Expr where
  fromInteger = Val . fromInteger
  negate (Val a) = Val (negate a)

instance Fractional Expr where
  fromRational = Val . fromRational

isVal :: Expr -> Bool
isVal (Val _) = True
isVal      _  = False

val :: Expr -> Double
val (Val x) = x
