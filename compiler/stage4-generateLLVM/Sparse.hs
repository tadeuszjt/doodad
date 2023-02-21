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


sparseTable :: InsCmp CompileState m => Pointer -> m Pointer
sparseTable sparse = do
    Sparse ts <- baseTypeOf sparse
    Pointer (Table ts) <$> gep (loc sparse) [int32 0, int32 0]
    

sparseStack :: InsCmp CompileState m => Pointer -> m Pointer
sparseStack sparse = do
    Sparse ts <- baseTypeOf sparse
    Pointer (Table [I64]) <$> gep (loc sparse) [int32 0, int32 1]


sparseDelete :: InsCmp CompileState m => Pointer -> Value -> m ()
sparseDelete sparse idx = do
    Sparse ts <- baseTypeOf sparse
    table <- sparseTable sparse
    ptrs <- tableColumn table idx
    forM_ ptrs $ \ptr -> do
        storeBasic ptr =<< newVal (typeof ptr)

    len <- pload =<< tableLen table
    idx64 <- convertNumber I64 idx
    idxIsEnd <- intInfix AST.EqEq idx64 =<< intInfix AST.Minus len (mkI64 1)
    if_ (op idxIsEnd) (tableShrink table) idxNotEndCase
    where
        idxNotEndCase :: InsCmp CompileState m => m ()
        idxNotEndCase = do
            stack <- sparseStack sparse
            len <- pload =<< tableLen stack
            tableResize stack =<< intInfix AST.Plus len (mkI64 1)
            [elm] <- tableColumn stack len
            storeBasicVal elm =<< convertNumber I64 idx

        tableShrink :: InsCmp CompileState m => Pointer -> m ()
        tableShrink table = do 
            len <- tableLen table 
            lenv <- pload len
            newLen <- intInfix AST.Minus lenv (mkI64 1)
            storeBasicVal len newLen
    
