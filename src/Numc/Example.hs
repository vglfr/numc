module Numc.Example where

import Numc.AST (Expr ((:+), (:-), (:*), (:/)))

v1 :: Expr
v1 = 5

b1 :: Expr
b1 = 1 :+ 2

b2 :: Expr
b2 = 1 :- 3 :+ 6 :* 2 :/ 4