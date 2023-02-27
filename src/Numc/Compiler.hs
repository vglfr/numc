{-# LANGUAGE OverloadedStrings #-}

module Numc.Compiler where

import Prelude hiding (div, putStrLn)

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Instruction (FAdd, FDiv, FMul, FSub)
  , Module
  , Name (Name, UnName)
  , Named ((:=), Do)
  , Operand (ConstantOperand, LocalReference)
  , Terminator (Ret)
  , defaultModule
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  , noFastMathFlags
  )
import LLVM.AST.Constant (Constant (Float))
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.Global (basicBlocks, functionDefaults, name, parameters, returnType)
import LLVM.AST.Type (double)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), Val), isVal, val)

add :: Operand -> Operand -> Word -> Named Instruction
add a b n = UnName n := FAdd noFastMathFlags a b []

sub :: Operand -> Operand -> Word -> Named Instruction
sub a b n = UnName n := FSub noFastMathFlags a b []

mul :: Operand -> Operand -> Word -> Named Instruction
mul a b n = UnName n := FMul noFastMathFlags a b []

div :: Operand -> Operand -> Word -> Named Instruction
div a b n = UnName n := FDiv noFastMathFlags a b []

eval :: Definition
eval = undefined

toVar :: Expr
toVar = undefined

toList :: Expr -> [Expr]
toList e = case e of
             a :+ b -> toList a <> toList b <> [e]
             a :- b -> toList a <> toList b <> [e]
             a :* b -> toList a <> toList b <> [e]
             a :/ b -> toList a <> toList b <> [e]
             Val _  -> []
             _ -> error "fook"

toInstr :: [Expr] -> Expr -> Named Instruction
toInstr es e = case e of
                 a :+ b -> add (getVal a) (getVal b) (toEnum . fromJust $ elemIndex e es)
                 a :- b -> sub (getVal a) (getVal b) (toEnum . fromJust $ elemIndex e es)
                 a :* b -> mul (getVal a) (getVal b) (toEnum . fromJust $ elemIndex e es)
                 a :/ b -> div (getVal a) (getVal b) (toEnum . fromJust $ elemIndex e es)
                 _ -> error "fook"
 where
  getVal e' = case e' of
                Val v -> constVal v
                _     -> localVal $ elemIndex e' es
  constVal = ConstantOperand . Float . Double
  localVal = LocalReference double . UnName . toEnum . fromJust

toDef :: Expr -> [Named Instruction] -> Definition
toDef e is = GlobalDefinition functionDefaults
  {
    name = Name "eval"
  , parameters = ([], False)
  , returnType = double
  , basicBlocks = [if isVal e then e2 else e1]
  }
 where
  e1 = BasicBlock
    ( Name "" )
    is
    ( Do $ Ret (Just . LocalReference double . UnName . toEnum . subtract 1 $ length is) [] )
  e2 = BasicBlock
    ( Name "" )
    is
    ( Do $ Ret (Just . ConstantOperand . Float . Double $ val e) [] )

toMod :: [Definition] -> Module
toMod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = ds
  }

compile :: [Expr] -> Module
compile = undefined
-- compile es = let ls = fmap genLine es
--               in toMod . toDef e $ fmap (toInstr es) es
--  where
--   genLine = undefined
  -- toLL e = let es = toList e
  --           in toMod . addBoilerplate . toDef e $ fmap (toInstr es) es
