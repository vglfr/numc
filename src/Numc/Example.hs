{-# LANGUAGE OverloadedStrings #-}

module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Fun))

val1 :: Expr
val1 = 5

val2 :: Expr
val2 = 5.5

val3 :: Expr
val3 = -5

{-
  +
 / \
1   2
-}
b1 :: Expr
b1 = 1 :+ 2

b2 :: Expr
b2 = 1 :- 2

b3 :: Expr
b3 = 1 :* 2

b4 :: Expr
b4 = 1 :/ 2

{-
       +        =>       5
     /   \             /   \
    -     /           2     4
   / \   / \         / \   / \
  +   3 *   4       1     3
 / \   / \         / \   / \
5   1 6   2
-}
b20 :: Expr
b20 = 5 :+ 1 :- 3 :+ 6 :* 2 :/ 4

var1 :: Expr
var1 = "x"

a1 :: Expr
a1 = "x" := 5

{-
x = 5;
x / 2;
y = x * 2;
x + 1 - y
-}
m2 :: [Expr]
m2 =
  [
    "x" := 5
  , "x" :/ 2
  , "y" := "x" :* 2
  , "x" :+ 1 :- "y"
  ]

{-
f x = (x + x) * x
-}
f1 :: Expr
f1 = Fun (pure "x") (("x" :+ "x") :* "x")
