{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.IRBuilder where

import Data.Text.Lazy.IO as T

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import Debug.Trace
import qualified LLVM.AST as AST

import Lang.JIT

simple :: IO ()
simple = do
 T.putStrLn $ ppllvm codegen
 runJIT codegen "add"

codegen :: AST.Module
codegen = do
  buildModule "test" $ mdo
    function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
      entry <- block `named` "entry"; do
        let arrayTy = Type.ArrayType 8 Type.i8
            idx0 = AST.ConstantOperand (C.Int 32 0)
            idx1 = AST.ConstantOperand (C.Int 32 7)
        ptr <- alloca arrayTy Nothing 0
        ep <- gep ptr [idx0,idx1]
        store ep 0 (AST.ConstantOperand (C.Int 8 97))
        e <- load ep 0
        ret e