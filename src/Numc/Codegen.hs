{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Numc.Codegen where

import Prelude hiding (div, putStrLn)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (putStrLn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Foreign.Ptr (FunPtr, castFunPtr)

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
import LLVM.Context (withContext)
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), Val))
import Numc.Example (b2)

foreign import ccall "dynamic" evalFFI :: FunPtr (IO Double) -> IO Double

add :: Operand -> Operand -> Word -> Named Instruction
add a b n = UnName n := FAdd noFastMathFlags a b []

sub :: Operand -> Operand -> Word -> Named Instruction
sub a b n = UnName n := FSub noFastMathFlags a b []

mul :: Operand -> Operand -> Word -> Named Instruction
mul a b n = UnName n := FMul noFastMathFlags a b []

div :: Operand -> Operand -> Word -> Named Instruction
div a b n = UnName n := FDiv noFastMathFlags a b []

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

toDef :: [Named Instruction] -> Definition
toDef is = GlobalDefinition functionDefaults
  {
    name = Name "eval"
  , parameters = ([], False)
  , returnType = double
  , basicBlocks = [e1]
  }
 where
  e1 = BasicBlock
    ( Name "" )
    is
    ( Do $ Ret (Just . LocalReference double . UnName . toEnum . subtract 1 $ length is) [] )

toMod :: Definition -> Module
toMod d = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = [d]
  }

toIR :: Module -> IO ByteString
toIR m = withContext $ \c -> withModuleFromAST c m moduleLLVMAssembly

runEval :: Module -> IO Double
runEval m = withContext $
  \c -> withMCJIT c (Just 2) Nothing Nothing Nothing $
    \e -> withModuleFromAST c m $
      \m' -> withModuleInEngine e m' $
        \e' -> do
          eval' <- getFunction e' (Name "eval")
          case eval' of
            Just f  -> evalFFI . castFunPtr $ f
            Nothing -> error "fook"

printAST :: IO ()
printAST = let es = toList b2
            in (toIR . toMod . toDef $ fmap (toInstr es) es) >>= putStrLn

evalRepl :: IO ()
evalRepl = let es = toList b2
            in (runEval . toMod . toDef $ fmap (toInstr es) es) >>= print
