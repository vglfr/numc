{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Numc.JIT where

import Prelude hiding (div, putStrLn)

import Foreign.Ptr (FunPtr, castFunPtr)

import LLVM.AST (Module, Name (Name))
import LLVM.Context (withContext)
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module (withModuleFromAST)

foreign import ccall "dynamic" evalFFI :: FunPtr (IO Double) -> IO Double

type Context = [String]

context :: Module -> Context -> Module
context = const

jit :: Module -> IO Double
jit m = withContext $
  \c -> withMCJIT c (Just 2) Nothing Nothing Nothing $
    \e -> withModuleFromAST c m $
      \m' -> withModuleInEngine e m' $
        \e' -> do
          eval' <- getFunction e' (Name "eval")
          case eval' of
            Just f  -> evalFFI . castFunPtr $ f
            Nothing -> error "fook"

-- eval :: Expr -> IO Double
-- eval e = let es = toList e
--           in runEval . toMod . pure . toDef e $ fmap (toInstr es) es
