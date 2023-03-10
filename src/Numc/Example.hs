{-# LANGUAGE OverloadedStrings #-}

module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Fun))

{- 5 -}
val1 :: Expr
val1 = 5

{- 5.5 -}
val2 :: Expr
val2 = 5.5

{- -5 -}
val3 :: Expr
val3 = -5

{- 1 + 2 -}
b1 :: Expr
b1 = 1 :+ 2

{- 1 - 2 -}
b2 :: Expr
b2 = 1 :- 2

{- 1 * 2 -}
b3 :: Expr
b3 = 1 :* 2

{- 1 / 2 -}
b4 :: Expr
b4 = 1 :/ 2

{- 5 + 1 - 3 + 6 * 2 / 4 -}
b20 :: Expr
b20 = 5 :+ 1 :- 3 :+ 6 :* 2 :/ 4

{- x -}
var1 :: Expr
var1 = "x"

{- x = 5 -}
a1 :: Expr
a1 = "x" := 5

{- x = 5 + 1 / 4 - 3 * 2 -}
a2 :: Expr
a2 = "x" := 5 :+ 1 :/ 4 :- 3 :* 2

{-
4 - 3;
1 + 2
-}
m1 :: [Expr]
m1 =
  [
    4 :- 3
  , 1 :+ 2
  ]

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
x = 5;
3 * 2 / 4 - 6;
y = 3;
3 - 4 + 6
-}
m3 :: [Expr]
m3 =
  [
    "x" := 5
  , 3 :* 2 :/ 4 :- 6
  , "y" := 3
  , 3 :- 4 :+ 6
  ]

{-
x = 5 + 2;
3 * 2 / 4 - 6;
y = 3 * 2;
3 - 4 + 6
-}
m4 :: [Expr]
m4 =
  [
    "x" := 5 :+ 2
  , 3 :* 2 :/ 4 :- 6
  , "y" := 3 :* 2
  , 3 :- 4 :+ 6
  ]

{-
x = 5;
x * 2
-}
m5 :: [Expr]
m5 =
  [
    "x" := 5
  , "x" :* 2
  ]

{-
x = 5 + 3;
x = x * 2;
x / 5
-}
m6 :: [Expr]
m6 =
  [
    "x" := 5 :+ 3
  , "x" := "x" :* 2
  , "x" :/ 5
  ]

{-
f x = (x + x) * x
-}
f1 :: Expr
f1 = Fun (pure "x") (("x" :+ "x") :* "x")
