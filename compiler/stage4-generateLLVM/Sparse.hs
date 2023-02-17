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


sparsePush :: InsCmp CompileState m => Pointer -> [Value] -> m Value2
sparsePush sparse elems = do
    Sparse ts <- baseTypeOf (typeof sparse)
    assert (map typeof elems == ts) "Elem types do not match"
    stack <- sparseStack sparse
    stackLen <- pload =<< tableLen stack
    stackLenGTZero <- intInfix AST.GT stackLen =<< pload =<< newI64 0
    ret <- newI64 0 
    if_ (op stackLenGTZero) (popStackCase stack ret) (pushTableCase ret) 
    pload ret
    where
        popStackCase :: InsCmp CompileState m => Pointer -> Pointer -> m ()
        popStackCase stack ret = do
            [idx] <- mkTablePop stack
            table <- sparseTable sparse
            column <- tableColumn table . toValue =<< valLoad idx
            forM_ (zip column elems) $ \(Pointer t p, elem) -> valStore (Ptr t p) elem
            valStore (fromPointer ret) idx
            
        pushTableCase :: InsCmp CompileState m => Pointer -> m ()
        pushTableCase ret = do
            table <- sparseTable sparse
            len <- pload =<< tableLen table
            tableResize table =<< intInfix AST.Plus len =<< pload =<< newI64 1
            ptrs <- tableColumn table len 
            forM_ (zip ptrs elems) $ \(Pointer t p, e) ->
                valStore (Ptr t p) e
            valStore (fromPointer ret) (fromValue len)


sparseDelete :: InsCmp CompileState m => Pointer -> Value2 -> m ()
sparseDelete sparse idx = do
    Sparse ts <- baseTypeOf (typeof sparse)
    table <- sparseTable sparse
    ptrs <- tableColumn table idx
    forM_ ptrs $ \ptr -> do
        storeBasic ptr =<< newVal (typeof ptr)

    len <- pload =<< tableLen table
    idxIsEnd <- intInfix AST.EqEq idx =<< intInfix AST.Minus len =<< pload =<< newI64 1
    if_ (op idxIsEnd) (void $ mkTablePop $ table) idxNotEndCase
    where
        idxNotEndCase :: InsCmp CompileState m => m ()
        idxNotEndCase = do
            stack <- sparseStack sparse
            len <- pload =<< tableLen stack
            tableResize stack =<< intInfix AST.Plus len =<< pload =<< newI64 1
            [elm] <- tableColumn stack len
            storeBasicVal elm idx
    
