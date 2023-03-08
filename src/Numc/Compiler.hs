{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.Compiler where

import Prelude hiding (div, mod, putStrLn)

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import qualified LLVM.AST (Named ((:=)))

import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Global (Function)
  , Instruction (FAdd, FDiv, FMul, FSub)
  , Module
  , Name (Name, UnName)
  , Named (Do)
  , Operand (ConstantOperand, LocalReference)
  , Terminator (Ret)
  , Type (FunctionType, VoidType, FloatingPointType)
  , defaultModule
  , mkName
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  , noFastMathFlags
  )
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (Float, GlobalReference))
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.Global (
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, isConstant, linkage, name, parameters
  , returnType, type'
  )
import LLVM.AST.Instruction (Instruction (Call, Load, Store))
import LLVM.AST.Linkage (Linkage (Private))
import LLVM.AST.Type (double, ptr, void)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Val, Var), isVal, val, var)

index :: (Eq a, Enum b) => a -> [a] -> b
index x xs = toEnum . fromJust $ elemIndex x xs

fname :: Show a => a -> Name
fname n = mkName $ "f" <> show n

isF :: Definition -> Bool
isF d = case d of
          GlobalDefinition (Function {}) -> True
          _ -> False

getF :: Definition -> Global
getF d = case d of
           GlobalDefinition f@(Function {}) -> f
           _ -> error "fook"

global :: Expr -> Expr -> Definition
global v e = GlobalDefinition globalVariableDefaults
  {
    name = mkName . var $ v
  , linkage = Private
  , isConstant = False
  , initializer = Just . Float . Double $ if isVal e then val e else 0
  , type' = double
  }

add :: Operand -> Operand -> Word -> Named Instruction
add a b n = UnName n LLVM.AST.:= FAdd noFastMathFlags a b []

sub :: Operand -> Operand -> Word -> Named Instruction
sub a b n = UnName n LLVM.AST.:= FSub noFastMathFlags a b []

mul :: Operand -> Operand -> Word -> Named Instruction
mul a b n = UnName n LLVM.AST.:= FMul noFastMathFlags a b []

div :: Operand -> Operand -> Word -> Named Instruction
div a b n = UnName n LLVM.AST.:= FDiv noFastMathFlags a b []

call :: Definition -> Word -> Named Instruction
call d n = let t = returnType . getF $ d
            in case t of
                 FloatingPointType _ -> UnName n LLVM.AST.:= Call Nothing C [] (function t) [] [] []
                 VoidType -> Do $ Call Nothing C [] (function t) [] [] []
                 _ -> error "fook"
 where
  function t = Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType t [] False) (getName d)
  getName = name . getF

store :: Int -> Expr -> Named Instruction
store n e = Do $ Store False (to e) (from n) Nothing 0 []
 where
  to = ConstantOperand . GlobalReference (ptr double) . mkName . var
  from = LocalReference double . UnName . toEnum

load :: Expr -> Word -> Named Instruction
load e n = UnName n LLVM.AST.:= Load False (ConstantOperand . GlobalReference (ptr double) . mkName . var $ e) Nothing 0 []

define :: Name -> Type -> [Named Instruction] -> Maybe Operand -> Definition
define n r is t = GlobalDefinition functionDefaults
  {
    name = n
  , parameters = ([], False)
  , returnType = r
  , basicBlocks = pure $ BasicBlock (Name "") is (Do $ Ret t [])
  }

eval :: [Definition] -> Definition
eval ds = GlobalDefinition functionDefaults
  {
    name = mkName "eval"
  , parameters = ([], False)
  , returnType = double
  , basicBlocks = pure $ BasicBlock (Name "") is e
  }
 where
  fs = filter isF ds
  is = uncurry call <$> zip fs (toEnum <$> tail [0..length fs])
  e = Do $ Ret (Just . LocalReference double . UnName . toEnum $ length fs) []

toList :: Expr -> [Expr]
toList e = case e of
             a :+ b -> toList a <> toList b <> [e]
             a :- b -> toList a <> toList b <> [e]
             a :* b -> toList a <> toList b <> [e]
             a :/ b -> toList a <> toList b <> [e]
             _ := b ->             toList b <> [e]
             Var _  -> [e]
             Val _  -> []
             _ -> error "fook"

instrs :: [Expr] -> [Named Instruction]
instrs es = fmap instr es
 where
  instr e = case e of
              a :+ b -> add (getVal a) (getVal b) (index e es)
              a :- b -> sub (getVal a) (getVal b) (index e es)
              a :* b -> mul (getVal a) (getVal b) (index e es)
              a :/ b -> div (getVal a) (getVal b) (index e es)
              a := _ -> store (index e es - 1) a
              Var _  -> load e (index e es)
              _ -> error "fook"
  getVal e = case e of
               Val v -> constVal v
               _     -> localVal $ index e es
  constVal = ConstantOperand . Float . Double
  localVal = LocalReference double . UnName

ass :: Expr -> Int -> [Definition]
ass e@(v := w) n = let m  = fname (n + 1)
                       is = instrs . toList $ e
                       t  = Nothing
                    in global v w : [define m void is t | not $ isVal w]

bin :: Expr -> Int -> Definition
bin e n = let m  = fname (n + 1)
              is = instrs . toList $ e
              t  = Just $ if isVal e
                          then ConstantOperand . Float . Double $ val e
                          else LocalReference double . UnName . toEnum . subtract 1 $ length is
           in define m double is t

mod :: [[Definition]] -> Module
mod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = concat ds -- <> pure (eval ds)
  }

compile :: [Expr] -> Module
compile es = mod . fmap compileLine $ es
 where
  compileLine e = let n = index e es
                   in case e of
                        _ :+ _ -> pure $ bin e n
                        _ :- _ -> pure $ bin e n
                        _ :* _ -> pure $ bin e n
                        _ :/ _ -> pure $ bin e n
                        _ := _ -> ass e n
                        -- Val _  -> undefined
                        -- Var _  -> undefined
                        _ -> error "fook"
