{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.Compiler where

import Prelude hiding (div, mod, putStrLn)

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.String (fromString)

import qualified LLVM.AST (Named ((:=)))

import Data.ByteString.Short (ShortByteString, toShort)
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
import LLVM.AST.Instruction (Instruction (Call, Store))
import LLVM.AST.Linkage (Linkage (Private))
import LLVM.AST.Type (double, ptr, void)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Val), isVal, val, var)

index :: (Eq a, Enum b) => a -> [a] -> b
index x xs = toEnum . fromJust $ elemIndex x xs

fname :: Show a => a -> ShortByteString
fname n = "f" <> (toShort . fromString . show $ n)

isF :: Definition -> Bool
isF d = case d of
          GlobalDefinition (Function {}) -> True
          _ -> False

getF :: Definition -> Global
getF d = case d of
           GlobalDefinition f@(Function {}) -> f
           _ -> error "fook"

variableD :: Expr -> Expr -> Definition
variableD s v = GlobalDefinition globalVariableDefaults
  {
    name = mkName . var $ s
  , linkage = Private
  , isConstant = False
  , initializer = Just . Float . Double $ val v
  , type' = double
  }

variableU :: Expr -> Definition
variableU s = GlobalDefinition globalVariableDefaults
  {
    name = mkName . var $ s
  , linkage = Private
  , isConstant = False
  , initializer = Just $ Float $ Double 0
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

store :: Named Instruction -> Expr -> Named Instruction
store i e = Do $ Store False (to e) (from i) Nothing 0 []
 where
  to = ConstantOperand . GlobalReference (ptr double) . mkName . var
  from (n LLVM.AST.:= _) = LocalReference double n

defineA :: Expr -> Int -> [Named Instruction] -> Definition
defineA v n is = GlobalDefinition functionDefaults
  {
    name = Name $ fname (n + 1)
  , parameters = ([], False)
  , returnType = void
  , basicBlocks = pure $ BasicBlock (Name "") (is <> pure (store (last is) v)) (Do $ Ret Nothing [])
  }

defineB :: Expr -> Int -> [Named Instruction] -> Definition
defineB e n is = GlobalDefinition functionDefaults
  {
    name = Name $ fname (n + 1)
  , parameters = ([], False)
  , returnType = double
  , basicBlocks = pure $ BasicBlock (Name "") is (if isVal e then e2 else e1)
  }
 where
  e1 = Do $ Ret (Just . LocalReference double . UnName . toEnum . subtract 1 $ length is) []
  e2 = Do $ Ret (Just . ConstantOperand . Float . Double $ val e) []

eval :: [Definition] -> Definition
eval ds = GlobalDefinition functionDefaults
  {
    name = Name "eval"
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
              _ -> error "fook"
  getVal e = case e of
               Val v -> constVal v
               _     -> localVal $ index e es
  constVal = ConstantOperand . Float . Double
  localVal = LocalReference double . UnName

ass :: Expr -> Int -> [Definition]
ass e n = case e of
            v := e' -> if isVal e'
                       then pure $ variableD v e'
                       else [variableU v, defineA v n $ instrs (toList e')]
            _ -> error "fook"

bin :: Expr -> Int -> Definition
bin e n = let es = toList e
           in defineB e n $ instrs es

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
                        _ -> error "fook"
