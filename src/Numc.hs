{-# LANGUAGE OverloadedStrings #-}

module Numc where

import Data.ByteString (ByteString)
import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Instruction (Add)
  , Module
  , Name (Name, UnName)
  , Named ((:=), Do)
  , Operand (ConstantOperand, LocalReference)
  , Parameter (Parameter)
  , Terminator (Ret)
  , Type (ArrayType)
  , defaultModule
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  )
import LLVM.AST.Constant (Constant (Array, Int))
import LLVM.AST.Global (
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, isConstant, linkage, name, parameters
  , returnType, type',
  )
import LLVM.AST.Linkage (Linkage (External, Private))
import LLVM.AST.Type (i8, i32, ptr, void)
import LLVM.Context (withContext)
import LLVM.AST.Instruction (Instruction (Call), tailCallKind)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

{-

@.fstr = private constant [2 x i8] c"%d"

declare i32 @printf(i8*, ...)

define i32 @add(i32 %a, i32 %b) {
  %1 = add i32 %a, %b
  ret i32 %1
}

define void @main() {
  %1 = getelementptr [2 x i8], [2 x i8]* @.fstr, i32 0, i32 0
  %2 = call i32 @add(i32 0, i32 97)

  call i32 (i8*, ...) @printf(i8* %1, i32 %2)
  ret void
}

-}

fstr :: Definition
fstr = GlobalDefinition globalVariableDefaults
  {
    name = Name ".fstr"
  , linkage = Private
  , isConstant = True
  , initializer = Just $ Array i8 [Int 8 37, Int 8 100] -- %d
  , type' = ArrayType 2 i8
  }

printf :: Definition
printf = GlobalDefinition functionDefaults
  {
    name = Name "printf"
  , linkage = External
  , parameters =
    ( [ Parameter (ptr i8) (UnName 0) [] ]
    , True ) -- set vararg
  , returnType = i32
  , basicBlocks = []
  }

add :: Definition
add = GlobalDefinition functionDefaults
  {
    name = Name "add"
  , parameters =
      ( [ Parameter i32 (Name "a") []
        , Parameter i32 (Name "b") [] ]
      , False )
  , returnType = i32
  , basicBlocks = [e1]
  }
 where
  e1 = BasicBlock
    ( Name "" )
    [ UnName 1 :=
        Add False  -- no   signed wrap
            False  -- no unsigned wrap
            (LocalReference i32 (Name "a"))
            (LocalReference i32 (Name "b"))
            [] ]
    ( Do $ Ret (Just $ LocalReference i32 $ UnName 1) [] )

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
        Add False  -- no   signed wrap
            False  -- no unsigned wrap
            (ConstantOperand $ Int 32 1)
            (ConstantOperand $ Int 32 1)
            []
    , UnName 2 :=
        Add False  -- no   signed wrap
            False  -- no unsigned wrap
            (ConstantOperand $ Int 32 1)
            (ConstantOperand $ Int 32 1)
            []
    , UnName 0 := Call undefined
      -- {
      --   tailCallKind = Nothing
      -- }
    ]
    ( Do $ Ret Nothing [] )

toMod :: [Definition] -> Module
toMod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = ds
  }

toIR :: Module -> IO ByteString
toIR m = withContext $ \c -> withModuleFromAST c m moduleLLVMAssembly





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




-- data Expr = Val Int | Op Op Int Int deriving Show

-- data Op = Ad | Sub | Mul | Div deriving Show

-- a :: Expr
-- a = Op Ad 1 2

-- parse :: IO ()
-- parse = print a

