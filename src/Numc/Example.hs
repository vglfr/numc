module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/)))

v1 :: Expr
v1 = 5

v2 :: Expr
v2 = 5.5

v3 :: Expr
v3 = -5

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