{-# LANGUAGE OverloadedStrings #-}

module Numc.Compiler where

import Prelude hiding (putStrLn)

import qualified Control.Monad as M (void)
import Data.ByteString.Char8 (putStrLn)
import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Module
  , Name (Name, UnName)
  , Named ((:=), Do)
  , Operand (ConstantOperand, LocalReference)
  , Parameter (Parameter)
  , Terminator (Ret)
  , Type (ArrayType, FunctionType)
  )
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (Array, GlobalReference, Int))
import LLVM.AST.Global (
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, isConstant, linkage, name, parameters
  , returnType, type',
  )
import LLVM.AST.Instruction (Instruction (Call, GetElementPtr))
import LLVM.AST.Linkage (Linkage (External, Private))
import LLVM.AST.Type (double, i8, i32, ptr, void)
import LLVM.Context (withContext)
import LLVM.Module (File (File), withModuleFromAST, writeObjectToFile)
import LLVM.Target (withHostTargetMachineDefault)

import System.Process (readProcess)

import Numc.Codegen (toList, toDef, toInstr, toMod, toIR)
import Numc.Example (b2)

fstr :: Definition
fstr = GlobalDefinition globalVariableDefaults
  {
    name = Name ".fstr"
  , linkage = Private
  , isConstant = True
  , initializer = Just $ Array i8 [Int 8 37, Int 8 102] -- %f
  , type' = ArrayType 2 i8
  }

printf :: Definition
printf = GlobalDefinition functionDefaults
  {
    name = Name "printf"
  , linkage = External
  , parameters =
    ( [ Parameter (ptr i8) (UnName 0) [] ]
    , True )
  , returnType = i32
  , basicBlocks = []
  }

main' :: Definition
main' = GlobalDefinition functionDefaults
  {
    name = Name "main"
  , returnType = void
  , basicBlocks = [body]
  }
 where
  body = BasicBlock
    ( Name "" )
    [ UnName 1 :=
        GetElementPtr False
                      (ConstantOperand $ GlobalReference (ptr $ ArrayType 2 i8) (Name ".fstr"))
                      [ ConstantOperand $ Int 32 0
                      , ConstantOperand $ Int 32 0 ]
                      []
    , UnName 2 :=
        Call Nothing
             C
             []
             (Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType double [] False) (Name "eval"))
             []
             []
             []
    , Do $
        Call Nothing
             C
             []
             (Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType i32 [ptr i8] True) (Name "printf"))
             [ (LocalReference (ptr i8) (UnName 1), [])
             , (LocalReference   double (UnName 2), []) ]
             []
             []
    ]
    ( Do $ Ret Nothing [] )

toBin :: Module -> IO ()
toBin m =
  withContext $ \c ->
    withModuleFromAST c m $ \m' ->
      withHostTargetMachineDefault $ \t -> do
        writeObjectToFile t (File "/tmp/numc.o") m'
        M.void $ readProcess "gcc" ["/tmp/numc.o"] ""

printAST :: IO ()
printAST = let es = toList b2
            in (toIR . toMod . (\x -> [fstr, printf, x, main']) . toDef $ fmap (toInstr es) es) >>= putStrLn

writeBin :: IO ()
writeBin = let es = toList b2
            in toBin . toMod . (\x -> [fstr, printf, x, main']) . toDef $ fmap (toInstr es) es
