{-# LANGUAGE FlexibleContexts #-}
module Sparse where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST.Instruction

import qualified AST
import Monad
import Value
import State
import Funcs
import Type 
import Tuple
import Typeof
import Trace
import Error
import Table
import Builtin



ptrSparseTable :: InsCmp CompileState m => Value -> m Value
ptrSparseTable val = do
    Sparse ts <- assertBaseType isSparse (valType val)
    assert (isPtr val) "val isnt pointer"
    Ptr (Table ts) <$> gep (valLoc val) [int32 0, int32 0]
    

ptrSparseStack :: InsCmp CompileState m => Value -> m Value
ptrSparseStack val = do
    Sparse ts <- assertBaseType isSparse (valType val)
    assert (isPtr val) "val isnt pointer"
    Ptr (Table [I64]) <$> gep (valLoc val) [int32 0, int32 1]


sparsePush :: InsCmp CompileState m => Value -> [Value] -> m Value
sparsePush val elems = do
    Sparse ts <- assertBaseType isSparse (valType val)
    assert (map valType elems == ts) "Elem types do not match"
    stack <- ptrSparseStack val
    stackLen <- mkTableLen stack
    stackLenGTZero <- mkIntInfix AST.GT stackLen (mkI64 0)
    ret <- mkAlloca I64
    if_ (valOp stackLenGTZero) (popStackCase stack ret) (pushTableCase ret) 
    return ret
    where
        popStackCase :: InsCmp CompileState m => Value -> Value -> m ()
        popStackCase stack ret = do
            [idx] <- mkTablePop stack
            table <- ptrSparseTable val
            column <- tableColumn table idx
            forM_ (zip column elems) $ \(Pointer t p, elem) -> valStore (Ptr t p) elem
            valStore ret idx
            
        pushTableCase :: InsCmp CompileState m => Value -> m ()
        pushTableCase ret = do
            table <- ptrSparseTable val
            len <- mkTableLen table
            tableResize table =<< mkIntInfix AST.Plus len (mkI64 1)
            ptrs <- tableColumn table len
            forM_ (zip ptrs elems) $ \(Pointer t p, e) ->
                valStore (Ptr t p) e
            valStore ret len 


sparseDelete :: InsCmp CompileState m => Value -> Value -> m ()
sparseDelete val idx = do
    Sparse ts <- assertBaseType isSparse (valType val)
    table <- ptrSparseTable val
    ptrs <- tableColumn table idx
    forM_ ptrs $ \(Pointer t p) -> do
        valStore (Ptr t p) =<< mkZero t

    len <- mkTableLen table
    idxIsEnd <- mkIntInfix AST.EqEq idx =<< mkIntInfix AST.Minus len (mkI64 1)
    if_ (valOp idxIsEnd) (void $ mkTablePop table) idxNotEndCase
    where
        idxNotEndCase :: InsCmp CompileState m => m ()
        idxNotEndCase = do
            stack <- ptrSparseStack val
            len <- mkTableLen stack
            tableResize stack =<< mkIntInfix AST.Plus len (mkI64 1)
            [Pointer t p] <- tableColumn stack len
            valStore (Ptr t p) idx
    
