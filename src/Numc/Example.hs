module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/)))

v1 :: Expr
v1 = 5

{-
  +
 / \
1   2
-}
b1 :: Expr
b1 = 1 :+ 2

{-
        /               4
       / \             / \
      *   4           3
     / \             / \
    +   2    =>     2
   / \             / \
  -   6           1
 / \             / \
1   3
-}
b2 :: Expr
b2 = 1 :- 3 :+ 6 :* 2 :/ 4