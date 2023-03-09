{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Numc.Compiler where

import Prelude hiding (div, mod, putStrLn)

import Data.ByteString.Short (unpack)
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
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, name, returnType, type'
  )
import LLVM.AST.Instruction (Instruction (Call, Load, Store))
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
getF (GlobalDefinition f@(Function {})) = f

global :: Expr -> Expr -> Definition
global v e = GlobalDefinition globalVariableDefaults
  {
    name = mkName . var $ v
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

call :: Definition -> Named Instruction
call d = let f = getF d
             t = returnType f
             n = name f
             v = case n of
                   Name n' -> fromIntegral . last . unpack $ n'
          in case t of
               FloatingPointType _ -> UnName v LLVM.AST.:= Call Nothing C [] (function t n) [] [] []
               VoidType -> Do $ Call Nothing C [] (function t n) [] [] []
 where
  function t n = Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType t [] False) n

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
  , returnType = r
  , basicBlocks = pure $ BasicBlock (Name "") is (Do $ Ret t [])
  }

eval :: [Definition] -> Definition
eval ds = GlobalDefinition functionDefaults
  {
    name = mkName "eval"
  , returnType = t
  , basicBlocks = pure $ BasicBlock (Name "") is (Do $ Ret r [])
  }
 where
  is = call <$> filter isF ds
  (t, r)
    | null is   = (void, Nothing)
    | otherwise = case last is of
                    (UnName n LLVM.AST.:= _) -> (double, Just . LocalReference double . UnName $ n)
                    _ -> (void, Nothing)

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

mod :: [Definition] -> Module
mod ds = defaultModule
  {
    moduleName = "mn\nff"
  , moduleSourceFileName = ""
  , moduleDefinitions = ds <> pure (eval ds)
  }

compile :: [Expr] -> Module
compile es = mod . concatMap compileLine $ es
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
