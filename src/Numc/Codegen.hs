{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Numc.Codegen where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (putStrLn)
import Foreign.Ptr (FunPtr, castFunPtr)

import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Instruction (Add, Mul, SDiv, Sub)
  , Module
  , Name (Name, UnName)
  , Named ((:=), Do)
  , Operand (ConstantOperand, LocalReference)
  , Terminator (Ret)
  , defaultModule
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  )
import LLVM.AST.Constant (Constant (Int))
import LLVM.AST.Global (basicBlocks, functionDefaults, name, parameters , returnType)
import LLVM.AST.Type (i32)
import LLVM.Context (Context, withContext)
import LLVM.ExecutionEngine (MCJIT, getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

import Prelude hiding (putStrLn)

import Numc.AST (Expr ((:+)))

foreign import ccall "dynamic" ffiAdd :: FunPtr (IO Int) -> IO Int

constVal :: Integer -> Operand
constVal = ConstantOperand . Int 32

localVal :: Word -> Operand
localVal = LocalReference i32 . UnName

add :: Operand -> Operand -> Word -> Named Instruction
add a b n = UnName n := Add False False a b []

sub :: Operand -> Operand -> Word -> Named Instruction
sub a b n = UnName n := Sub False False a b []

mul :: Operand -> Operand -> Word -> Named Instruction
mul a b n = UnName n := Mul False False a b []

div :: Operand -> Operand -> Word -> Named Instruction
div a b n = UnName n := SDiv      False a b []

toInstr :: Expr -> [Named Instruction]
toInstr e = case e of
              a :+ b -> add (constVal a) (constVal b) 0 : concatMap toInstr [a, b]
              _ -> error "fook"
  -- [add (constVal 5) (constVal 6) 1]

expr :: [Named Instruction] -> Definition
expr is = GlobalDefinition functionDefaults
  {
    name = Name "expr"
  , parameters = ([], False)
  , returnType = i32
  , basicBlocks = [e1]
  }
 where
  e1 = BasicBlock
    ( Name "" )
    is
    ( Do $ Ret (Just $ LocalReference i32 $ UnName . toEnum $ length is) [] )

toMod :: [Definition] -> Module
toMod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = ds
  }

toIR :: Module -> IO ByteString
toIR m = withContext $ \c -> withModuleFromAST c m moduleLLVMAssembly

jit :: Context -> (MCJIT -> IO a) -> IO a
jit c = withMCJIT c optlevel model ptrelim fastins
 where
  optlevel = Just 2  -- optimization level
  model    = Nothing -- code model ( Default )
  ptrelim  = Nothing -- frame pointer elimination
  fastins  = Nothing -- fast instruction selection

runAdd' :: Module -> IO Int
runAdd' m = withContext $
  \c -> jit c $
    \e -> withModuleFromAST c m $
      \m' -> withModuleInEngine e m' $
        \e' -> do
          mainfn <- getFunction e' (Name "add")
          case mainfn of
            Just f  -> ffiAdd . castFunPtr $ f
            Nothing -> error "fook"

printAST :: IO ()
printAST = (toIR . toMod $ [expr . toInstr $ undefined]) >>= putStrLn

evalRepl :: IO ()
evalRepl = (runAdd' . toMod $ [expr . toInstr $ undefined]) >>= print
