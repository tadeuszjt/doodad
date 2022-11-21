{-# LANGUAGE FlexibleContexts #-}
module Sparse where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST.Instruction

import qualified AST as S
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



sparseTable :: InsCmp CompileState m => Value -> m Value
sparseTable val = do
    Sparse ts <- assertBaseType isSparse (valType val)
    case val of
        Val _ op -> Val (Table ts) <$> extractValue op [0]
        Ptr _ loc -> Ptr (Table ts) <$> gep loc [int32 0, int32 0]
    

sparseStack :: InsCmp CompileState m => Value -> m Value
sparseStack val = do
    Sparse ts <- assertBaseType isSparse (valType val)
    case val of
        Val _ op -> Val (Table [I64]) <$> extractValue op [1]
        Ptr _ loc -> Ptr (Table [I64]) <$> gep loc [int32 0, int32 1]


--sparseAppendElem :: InsCmp CompileState m => Value -> Value -> m Value
--sparseAppendElem val elem = do
--    Sparse ts <- assertBaseType isSparse (valType val)
--    stack <- sparseStack val
--    stackLen <- tableLen stack
--    stackLenGTZero <- valIntInfix S.GT stackLen (valI64 0)
----    if_ (valOp stackLenGTZero)
----
--    where
--        popStackCase :: InsCmp CompileState m => Value -> m ()
--        popStackCase stack = do
--            idx <- tablePopElem stack
--            tableSetRowElem tab 
            
    



