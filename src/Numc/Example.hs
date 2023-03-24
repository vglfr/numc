{-# LANGUAGE OverloadedStrings #-}

module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Fun, Exe))

{- 5 -}
v1 :: Expr
v1 = 5

{- 5.5 -}
v2 :: Expr
v2 = 5.5

{- -5 -}
v3 :: Expr
v3 = -5

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

{- -1 + 2 -}
b5 :: Expr
b5 = -1 :+ 2

{- 1 - -2 -}
b6 :: Expr
b6 = 1 :- -2

{- 5 + 1 - 3 + 6 * 2 / 4 -}
b20 :: Expr
b20 = 5 :+ 1 :- 3 :+ 6 :* 2 :/ 4

{- x -}
w1 :: Expr
w1 = "x"

{- x = 5 -}
a1 :: Expr
a1 = "x" := 5

{- x = 5 + 1 / 4 - 3 * 2 -}
a2 :: Expr
a2 = "x" := 5 :+ 1 :/ 4 :- 3 :* 2

{- x = 5 + y -}
a3 :: Expr
a3 = "x" := 5 :+ "y"

{- x = 5 + f(y) -}
a4 :: Expr
a4 = "x" := 5 :+ Exe "f" ["y"]

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
f(z) { z * 2 };
x = f(4) * 2
-}
m7 :: [Expr]
m7 =
  [
    Fun "f" ["z"] ["z" :* 2]
  , "x" := Exe "f" [4] :* 2
  ]

{-
f(x) {
  (x + x) * x 
}
-}
f1 :: Expr
f1 = Fun "f" ["x"] [("x" :+ "x") :* "x"]

{-
f(x, y) {
  (x + y) * x / y 
}
-}
f2 :: Expr
f2 = Fun "f" ["x", "y"] [("x" :+ "y") :* "x" :/ "y"]

{-
f(x, y) {
  x + 2;
  x / y 
}
-}
f3 :: Expr
f3 = Fun "f" ["x", "y"] ["x" :+ 2, "x" :/ "y"]

{- f(5) -}
e1 :: Expr
e1 = Exe "f" [5]

{- f(5, 3) -}
e2 :: Expr
e2 = Exe "f" [5, 3]

{- f() -}
e3 :: Expr
e3 = Exe "f" []

{- f(5 + 3) -}
e4 :: Expr
e4 = Exe "f" [5 :+ 3]

{- f(5 + y) -}
e5 :: Expr
e5 = Exe "f" [5 :+ "y"]
