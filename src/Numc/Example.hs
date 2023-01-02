module Numc.Example where

import Numc.AST (Expr (Bin), Bin ((:+)))

v1 :: Expr
v1 = 5

b1 :: Expr
b1 = Bin (1 :+ 2)