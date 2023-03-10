{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Numc.JIT where

import Prelude hiding (div, putStrLn)

import Foreign.Ptr (FunPtr, castFunPtr)

import LLVM.AST (Module, mkName)
import LLVM.Context (withContext)
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module (withModuleFromAST)
import Control.Exception (try)
import LLVM.Exception (EncodeException (EncodeException))

foreign import ccall "dynamic" evalFFI :: FunPtr (IO Double) -> IO Double

type Context = [String]

jit :: Module -> IO String
jit m = withContext $
  \c -> withMCJIT c (Just 2) Nothing Nothing Nothing $
    \e -> do
      r <- try @EncodeException $ withModuleFromAST c m $
        \m' -> withModuleInEngine e m' $
          \e' -> do
            eval' <- getFunction e' (mkName "eval")
            case eval' of
              Just f  -> evalFFI . castFunPtr $ f
              Nothing -> error "fook"
      pure $ case r of
               Left (EncodeException s) -> s
               Right x -> show x
