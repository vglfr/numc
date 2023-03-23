module Numc.Codegen where

import Prelude hiding (writeFile)

import qualified Control.Monad as M (void)

import Data.ByteString (writeFile)
import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Module (moduleDefinitions)
  , Name (UnName)
  , Named ((:=), Do)
  , Operand (ConstantOperand, LocalReference)
  , Parameter (Parameter)
  , Terminator (Ret)
  , Type (ArrayType)
  )
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (Array, Int))
import LLVM.AST.Global (
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, linkage, name, parameters, returnType, type',
  )
import LLVM.AST.Instruction (Instruction (Call))
import LLVM.AST.Linkage (Linkage (External))
import LLVM.AST.Name (mkName)
import LLVM.AST.Type (double, i8, i32, ptr)
import LLVM.Context (withContext)
import LLVM.Module (File (File), moduleLLVMAssembly, withModuleFromAST, writeObjectToFile)
import LLVM.Target (withHostTargetMachineDefault)

import System.Process (readProcess)

import Numc.Compiler (getelementptr, isVoid, fptr)

fstr :: Definition
fstr = GlobalDefinition globalVariableDefaults
  {
    name = mkName ".fstr"
  , initializer = Just $ Array i8 [Int 8 37, Int 8 102] -- %f
  , type' = ArrayType 2 i8
  }

printf :: Definition
printf = GlobalDefinition functionDefaults
  {
    name = mkName "printf"
  , linkage = External
  , parameters = (pure $ Parameter (ptr i8) (UnName 0) [], True)
  , returnType = i32
  , basicBlocks = []
  }

main :: Module -> Definition
main m = GlobalDefinition functionDefaults
  {
    name = mkName "main"
  , returnType = i8
  , basicBlocks = pure $ BasicBlock (mkName "") body (Do $ Ret (Just $ ConstantOperand $ Int 8 0) [])
  }
 where
  body = if isVoid m
         then [] -- call void eval
         else 
           [
             UnName 1 := getelementptr (ArrayType 2 i8) ".fstr" 0
           , UnName 2 := Call Nothing C [] (Right $ fptr double [] (mkName "eval")) [] [] []
           , Do $ Call Nothing C [] (Right $ fptr i32 [ptr i8] (mkName "printf")) args [] []
           ]
  args = [ (LocalReference (ptr i8) (UnName 1), [])
         , (LocalReference   double (UnName 2), []) ]

boilerplate :: Module -> Module
boilerplate m = m { moduleDefinitions = [fstr, printf] <> moduleDefinitions m <> [main m] }

writeLL :: Module -> String -> IO ()
writeLL m p = toIR >>= writeFile p
 where
  toIR = withContext $ \c -> withModuleFromAST c m moduleLLVMAssembly

writeBin :: Module -> String -> IO ()
writeBin m s =
  withContext $ \c ->
    withModuleFromAST c m $ \m' ->
      withHostTargetMachineDefault $ \t -> do
        writeObjectToFile t (File "/tmp/numc.o") m'
        M.void $ readProcess "gcc" ["/tmp/numc.o", "-o", s] mempty
