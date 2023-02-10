{-# LANGUAGE FlexibleContexts #-}
module Tuple where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST
import Type
import State
import Monad
import Value
import Typeof
import Trace
import Error
import Symbol


tupleLength :: InsCmp CompileState m => Value -> m Int
tupleLength val = trace "tupleLength" $ do
    Tuple ts <- assertBaseType isTuple (valType val)
    return (length ts)


ptrTupleField :: InsCmp CompileState m => Symbol -> Value -> m Value
ptrTupleField symbol tup = trace "tupleField" $ do
    let typ = valType tup
    assert (isTypedef typ) "Cannot have member of raw tuple"
    assertBaseType isTuple typ
    ObjField i <- look symbol
    ptrTupleIdx i tup


ptrTupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
ptrTupleIdx i tup = withErrorPrefix "tuple idx: " $ do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (isPtr tup)               "tuple isnt pointer"
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Ptr (ts !! i) <$> gep (valLoc tup) [int32 0, int32 $ fromIntegral i]


valTupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
valTupleIdx i tup = do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Val (ts !! i) <$> case tup of
        Val _ op  -> extractValue op [fromIntegral $ i]
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 $ fromIntegral i]
