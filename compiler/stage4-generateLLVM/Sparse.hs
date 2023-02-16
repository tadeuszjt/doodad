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



sparseTable :: InsCmp CompileState m => Pointer -> m Pointer
sparseTable sparse = do
    Sparse ts <- baseTypeOf (typeof sparse)
    Pointer (Table ts) <$> gep (loc sparse) [int32 0, int32 0]
    

sparseStack :: InsCmp CompileState m => Pointer -> m Pointer
sparseStack sparse = do
    Sparse ts <- baseTypeOf (typeof sparse)
    Pointer (Table [I64]) <$> gep (loc sparse) [int32 0, int32 1]


sparsePush :: InsCmp CompileState m => Pointer -> [Value] -> m Value
sparsePush sparse elems = do
    Sparse ts <- baseTypeOf (typeof sparse)
    assert (map typeof elems == ts) "Elem types do not match"
    stack <- sparseStack sparse
    stackLen <- toVal =<< tableLen stack
    stackLenGTZero <- mkIntInfix AST.GT stackLen =<< toVal =<< newI64 0
    ret <- newI64 0 
    if_ (valOp stackLenGTZero) (popStackCase stack ret) (pushTableCase ret) 
    return (fromPointer ret)
    where
        popStackCase :: InsCmp CompileState m => Pointer -> Pointer -> m ()
        popStackCase stack ret = do
            [idx] <- mkTablePop stack
            table <- sparseTable sparse
            column <- tableColumn table idx
            forM_ (zip column elems) $ \(Pointer t p, elem) -> valStore (Ptr t p) elem
            valStore (fromPointer ret) idx
            
        pushTableCase :: InsCmp CompileState m => Pointer -> m ()
        pushTableCase ret = do
            table <- sparseTable sparse
            len <- toVal =<< tableLen table
            tableResize table =<< mkIntInfix AST.Plus len =<< toVal =<< newI64 1
            ptrs <- tableColumn table len
            forM_ (zip ptrs elems) $ \(Pointer t p, e) ->
                valStore (Ptr t p) e
            valStore (fromPointer ret) len 


sparseDelete :: InsCmp CompileState m => Value -> Value -> m ()
sparseDelete val idx = do
    Sparse ts <- assertBaseType isSparse (typeof val)
    table <- sparseTable (toPointer val)
    ptrs <- tableColumn table idx
    forM_ ptrs $ \ptr -> do
        storeBasic ptr =<< newVal (typeof ptr)

    len <- toVal =<< tableLen table
    idxIsEnd <- mkIntInfix AST.EqEq idx =<< mkIntInfix AST.Minus len =<< toVal =<< newI64 1
    if_ (valOp idxIsEnd) (void $ mkTablePop $ table) idxNotEndCase
    where
        idxNotEndCase :: InsCmp CompileState m => m ()
        idxNotEndCase = do
            stack <- sparseStack (toPointer val)
            len <- toVal =<< tableLen stack
            tableResize stack =<< mkIntInfix AST.Plus len =<< toVal =<< newI64 1
            [Pointer t p] <- tableColumn stack len
            valStore (Ptr t p) idx
    
