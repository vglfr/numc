{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Numc.JIT where

import Control.Exception (try)
import Data.Maybe (fromJust)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr)
import Foreign.Marshal.Array (peekArray)

import Data.ByteString.Short (fromShort)
import Data.ByteString.Internal (unpackChars)
import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Instruction (Store)
  , Module (moduleDefinitions)
  , Name (UnName, Name)
  , Named (Do, (:=))
  , Terminator (Ret)
  , Type (ArrayType)
  )
import LLVM.AST.Constant (Constant(Array, Float))
import LLVM.AST.Float (SomeFloat(Double))
import LLVM.AST.Global (basicBlocks, functionDefaults, globalVariableDefaults, initializer, name, returnType, type')
import LLVM.AST.Name (mkName)
import LLVM.AST.Type (double, ptr)
import LLVM.Context (withContext)
import LLVM.Exception (EncodeException)
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module (withModuleFromAST)

import Numc.Compiler (getInner, getelementptr, isF, load, lref)

foreign import capi "dynamic" eval :: FunPtr (IO Double) -> IO Double
foreign import capi "dynamic" context :: FunPtr (IO (Ptr Double)) -> IO (Ptr Double)

gs :: Int -> Definition
gs n = GlobalDefinition globalVariableDefaults
  {
    name = mkName ".gs"
  , initializer = Just $ Array double [Float $ Double 0]
  , type' = ArrayType (fromIntegral n) double
  }

ctx :: [Definition] -> Int -> Definition
ctx ds n = GlobalDefinition functionDefaults
  {
    name = mkName "ctx"
  , returnType = ptr double
  , basicBlocks = pure $ BasicBlock (mkName "") (concatMap body $ zip [0..] ds) (Do $ Ret (if n == 0 then Nothing else Just $ lref 0) [])
  }
 where
  body (i,d) = [
                 UnName (i*2)     := getelementptr (ArrayType (fromIntegral n) double) ".gs" (fromIntegral i)
               , UnName (i*2 + 1) := load (unpackChars . fromShort . sname . name . getInner $ d)
               , Do $ Store False (lref $ i*2) (lref $ i*2 + 1) Nothing 0 []
               ]
  sname (Name s) = s

mkContext :: Module -> [Definition]
mkContext m = let vs = filter (not . isF) . moduleDefinitions $ m
                  l  = length vs
               in [gs l, ctx vs l]

boilerplate :: Module -> Module
boilerplate m = m { moduleDefinitions = moduleDefinitions m <> mkContext m }

jit :: Module -> Int -> IO (Either EncodeException (Double, [Double]))
jit m n = withContext $
  \c -> withMCJIT c (Just 2) Nothing Nothing Nothing $
    \e -> try @EncodeException $ withModuleFromAST c m $
      \m' -> withModuleInEngine e m' $
        \e' -> do
          r <- getFunction e' (mkName "eval") >>= eval . castFunPtr . fromJust
          c' <- getFunction e' (mkName "ctx") >>= context . castFunPtr . fromJust >>= peekArray n
          pure (r, c')
