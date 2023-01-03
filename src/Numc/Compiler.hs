{-# LANGUAGE OverloadedStrings #-}

module Numc.Compiler where

import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
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
import LLVM.AST.Type (i8, i32, ptr, void)

import Prelude hiding (putStrLn)

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
             (Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType i32 [i32, i32] False) (Name "add"))
             [ (ConstantOperand $ Int 32 0, [])
             , (ConstantOperand $ Int 32 97, []) ]
             []
             []
    , Do $
        Call Nothing
             C
             []
             (Right $ ConstantOperand $ GlobalReference (ptr $ FunctionType i32 [ptr i8] True) (Name "printf"))
             [ (LocalReference (ptr i8) (UnName 1), [])
             , (LocalReference      i32 (UnName 2), []) ]
             []
             []
    ]
    ( Do $ Ret Nothing [] )

-- toObj :: AST.Module -> IO ()
-- toObj ast = do
--   withContext $ \ctx ->
--     withModuleFromAST ctx ast $ \llvm ->
--       withHostTargetMachineDefault $ \target -> do
--         writeObjectToFile target (File "bin/test.o") llvm

-- toBin :: AST.Module -> IO ()
-- toBin ast = do
--   withContext $ \ctx ->
--     withModuleFromAST ctx ast $ \llvm ->
--       withHostTargetMachineDefault $ \target -> do
--         writeObjectToFile target (File "bin/test.o") llvm
--         void $ readProcess "gcc" ["bin/test.o", "-o", "bin/a.out"] ""
