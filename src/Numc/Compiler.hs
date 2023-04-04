{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Numc.Compiler where

import Prelude hiding (div, mod, putStrLn)

import Data.ByteString (ByteString)
import Data.ByteString.Internal (unpackChars)
import Data.ByteString.Short (fromShort, unpack)
import Data.List (elemIndex, nub, find)
import Data.Maybe (fromJust)

import qualified LLVM.AST (Named ((:=)))

import LLVM.AST
  (
    BasicBlock (BasicBlock)
  , Definition (GlobalDefinition)
  , Global (Function)
  , Instruction (FAdd, FDiv, FMul, FSub, GetElementPtr)
  , Module
  , Name (Name, UnName)
  , Named (Do)
  , Operand (ConstantOperand, LocalReference)
  , Terminator (Ret)
  , Type (FunctionType, VoidType, FloatingPointType)
  , defaultModule
  , mkName
  , moduleDefinitions
  , moduleName
  , moduleSourceFileName
  , noFastMathFlags
  )
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (Float, GlobalReference, Int))
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.Global (
    basicBlocks, functionDefaults, globalVariableDefaults, initializer, name, returnType, type'
  )
import LLVM.AST.Instruction (Instruction (Call, Load, Store))
import LLVM.AST.Type (double, ptr, void)

import LLVM.Internal.Context (withContext)
import LLVM.Internal.Module (withModuleFromAST, moduleLLVMAssembly)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Exe, Fun, Val, Var), isVal, val, var)

index :: (Eq a, Enum b) => a -> [a] -> b
index x xs = toEnum . fromJust $ elemIndex x xs

fname :: Show a => a -> Name
fname n = mkName $ "f" <> show n

isF :: Definition -> Bool
isF d = case d of
          GlobalDefinition (Function {}) -> True
          _ -> False

vars :: Module -> [Expr]
vars = fmap expr . filter (/= ".gs") . fmap (name . getInner) . filter (not . isF) . moduleDefinitions
 where
  expr (Name n) = Var . unpackChars . fromShort $ n

getInner :: Definition -> Global
getInner (GlobalDefinition x) = x

global :: Expr -> Expr -> Definition
global v e = GlobalDefinition globalVariableDefaults
  {
    name = mkName . var $ v
  , initializer = Just . Float . Double $ if isVal e then val e else 0
  , type' = double
  }

fadd :: Operand -> Operand -> Instruction
fadd a b = FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Instruction
fsub a b = FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Instruction
fmul a b = FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Instruction
fdiv a b = FDiv noFastMathFlags a b []

fptr :: Type -> [Type] -> Bool -> Name -> Operand
fptr t ts v n = ConstantOperand $ GlobalReference (ptr $ FunctionType t ts v) n

gref :: String -> Operand
gref = ConstantOperand . GlobalReference (ptr double) . mkName

lref :: Word -> Operand
lref = LocalReference double . UnName

call :: Definition -> Named Instruction
call d = let f = getInner d
             t = returnType f
             n = name f
             v = case n of
                   Name n' -> fromIntegral . last . unpack $ n'
          in case t of
               FloatingPointType _ -> UnName v LLVM.AST.:= Call Nothing C [] (Right $ fptr t [] False n) [] [] []
               VoidType -> Do $ Call Nothing C [] (Right $ fptr t [] False n) [] [] []

store :: String -> Word -> Instruction
store g l = Store False (gref g) (lref l) Nothing 0 []

load :: String -> Instruction
load n = Load False (gref n) Nothing 0 []

getelementptr :: Type -> String -> Integer -> Instruction
getelementptr t n o = GetElementPtr
                        False
                        (ConstantOperand $ GlobalReference (ptr t) (mkName n))
                        [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 o]
                        []

define :: Name -> Type -> [Named Instruction] -> Maybe Operand -> Definition
define n r is t = GlobalDefinition functionDefaults
  {
    name = n
  , returnType = r
  , basicBlocks = pure $ BasicBlock (Name "") is (Do $ Ret t [])
  }

eval :: [Definition] -> Definition
eval ds = GlobalDefinition functionDefaults
  {
    name = mkName "eval"
  , returnType = t
  , basicBlocks = pure $ BasicBlock (Name "") is (Do $ Ret r [])
  }
 where
  is = call <$> filter isF ds
  (t, r)
    | null is   = (void, Nothing)
    | otherwise = case last is of
                    (UnName n LLVM.AST.:= _) -> (double, Just $ lref n)
                    _ -> (void, Nothing)

toList :: Expr -> [Expr]
toList e = case e of
             a :+ b -> toList a <> toList b <> [e]
             a :- b -> toList a <> toList b <> [e]
             a :* b -> toList a <> toList b <> [e]
             a :/ b -> toList a <> toList b <> [e]
             _ := b ->             toList b <> [e]
             Var _  -> [e]
             Val _  -> []
             _ -> error "fook"

instrs :: [Expr] -> [Named Instruction]
instrs es = fmap instr es
 where
  instr e = let n = index e es
             in case e of
                  a :+ b -> UnName n LLVM.AST.:= fadd (getVal a) (getVal b)
                  a :- b -> UnName n LLVM.AST.:= fsub (getVal a) (getVal b)
                  a :* b -> UnName n LLVM.AST.:= fmul (getVal a) (getVal b)
                  a :/ b -> UnName n LLVM.AST.:= fdiv (getVal a) (getVal b)
                  a := _ -> Do $ store (var a) (n - 1)
                  Var _  -> UnName n LLVM.AST.:= load (var e)
                  _ -> error "fook"
  getVal e = case e of
               Val v -> constVal v
               _     -> lref $ index e es
  constVal = ConstantOperand . Float . Double

variable :: Expr -> Int -> Definition
variable e n = define (fname n) double [UnName 0 LLVM.AST.:= load (var e)] (Just $ lref 0)

ass :: Expr -> Int -> [Definition]
ass e@(v := w) n = let m  = fname (n + 1)
                       is = instrs . toList $ e
                       t  = Nothing
                    in global v w : [define m void is t | not $ isVal w]

bin :: Expr -> Int -> Definition
bin e n = let m  = fname (n + 1)
              is = instrs . toList $ e
              t  = Just $ if isVal e
                          then ConstantOperand . Float . Double $ val e
                          else lref . toEnum . subtract 1 $ length is
           in define m double is t

mod :: [Definition] -> Module
mod ds = defaultModule
  {
    moduleName = ""
  , moduleSourceFileName = ""
  , moduleDefinitions = ds <> pure (eval ds)
  }

isVoid :: Module -> Bool
isVoid = (void ==) . returnType . getInner . fromJust . find (isEval . name . getInner) . moduleDefinitions
 where
  isEval (Name n) = n == "eval"
  

compile :: [Expr] -> Module
compile es = mod . nub . concatMap compileLine $ es
 where
  compileLine e = let n = index e es
                   in case e of
                        _ :+ _ -> pure $ bin e n
                        _ :- _ -> pure $ bin e n
                        _ :* _ -> pure $ bin e n
                        _ :/ _ -> pure $ bin e n
                        _ := _ -> ass e n
                        Val _  -> pure $ bin e n
                        Var _  -> pure $ variable e n
                        Fun {} -> error "fook"
                        Exe {} -> error "fook"

ir :: Module -> IO ByteString
ir m = withContext $ \c -> withModuleFromAST c m moduleLLVMAssembly