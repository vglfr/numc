{-# LANGUAGE OverloadedStrings #-}

module Numc.Compiler where

import Prelude hiding (div, mod, putStrLn)

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.String (fromString)

import qualified LLVM.AST (Named ((:=)))

import Data.ByteString.Short (toShort, ShortByteString)
import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Instruction (FAdd, FDiv, FMul, FSub)
  , Module
  , Name (Name, UnName)
  , Named (Do)
  , Operand (ConstantOperand, LocalReference)
  , Terminator (Ret)
  , Type (FunctionType)
  , defaultModule
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  , noFastMathFlags
  )
import LLVM.AST.Constant (Constant (Float, GlobalReference))
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.Global (basicBlocks, functionDefaults, name, parameters, returnType)
import LLVM.AST.Type (double, ptr)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Val), isVal, val)
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Instruction (Instruction(Call))

index :: (Eq a, Enum b) => a -> [a] -> b
index x xs = toEnum . fromJust $ elemIndex x xs

fname :: Show a => a -> ShortByteString
fname n = "f" <> (toShort . fromString . show $ n)

add :: Operand -> Operand -> Word -> Named Instruction
add a b n = UnName n LLVM.AST.:= FAdd noFastMathFlags a b []

sub :: Operand -> Operand -> Word -> Named Instruction
sub a b n = UnName n LLVM.AST.:= FSub noFastMathFlags a b []

mul :: Operand -> Operand -> Word -> Named Instruction
mul a b n = UnName n LLVM.AST.:= FMul noFastMathFlags a b []

div :: Operand -> Operand -> Word -> Named Instruction
div a b n = UnName n LLVM.AST.:= FDiv noFastMathFlags a b []

call :: Word -> Named Instruction
call n = UnName n LLVM.AST.:= Call Nothing C [] f [] [] []
 where
  f = Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType double [] False) (Name $ fname n)

define :: Expr -> Int -> [Named Instruction] -> Definition
define e n is = GlobalDefinition functionDefaults
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
  is = call . toEnum <$> tail [0..length ds]
  e = Do $ Ret (Just . LocalReference double . UnName . toEnum $ length ds) []

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
                 a :+ b -> add (getVal a) (getVal b) (index e es)
                 a :- b -> sub (getVal a) (getVal b) (index e es)
                 a :* b -> mul (getVal a) (getVal b) (index e es)
                 a :/ b -> div (getVal a) (getVal b) (index e es)
                 _ -> error "fook"
 where
  getVal e' = case e' of
                Val v -> constVal v
                _     -> localVal $ index e' es
  constVal = ConstantOperand . Float . Double
  localVal = LocalReference double . UnName

ass :: Expr -> [Definition]
ass = undefined

bin :: Expr -> Int -> [Definition]
bin e n = let es = toList e
           in pure $ define e n $ fmap (toInstr es) es

mod :: [Definition] -> Module
mod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = ds <> pure (eval ds)
  }

compile :: [Expr] -> Module
compile es = mod . concatMap compileLine $ es
 where
  compileLine e = case e of
                    _ :+ _ -> bin e $ index e es
                    _ :- _ -> bin e $ index e es
                    _ :* _ -> bin e $ index e es
                    _ :/ _ -> bin e $ index e es
                    _ := _ -> ass e
                    _ -> error "fook"
