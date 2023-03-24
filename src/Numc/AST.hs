{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.AST where

import Data.List (intersperse)
import Data.String (IsString, fromString)
import GHC.Show (showCommaSpace)

data Expr
  = Var String
  | Val Double
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr := Expr
  | Fun Expr [Expr] [Expr]
  | Exe Expr [Expr]
  deriving Eq

infixl 5 :+
infixl 5 :-
infixl 6 :*
infixl 6 :/
infixl 1 :=

instance Show Expr where
  showsPrec n e = case e of
                    Var v -> showString v
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
                    Fun f as es -> shows f . showParen True (showsArgs as) . showBrace (showsExpr es)
                    Exe f as    -> shows f . showParen True (showsArgs as)
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

showsArgs :: [Expr] -> ShowS
showsArgs = foldr (.) id . intersperse showCommaSpace . fmap shows

showsExpr :: [Expr] -> ShowS
showsExpr = foldr (.) id . intersperse (showString "; ") . fmap shows

showBrace :: ShowS -> ShowS
showBrace s = showString " { " . s . showString " }"

isVal :: Expr -> Bool
isVal (Val _) = True
isVal      _  = False

val :: Expr -> Double
val (Val x) = x

var :: Expr -> String
var (Var x) = x
