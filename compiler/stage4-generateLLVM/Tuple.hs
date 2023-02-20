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
tupleLength tuple = trace "tupleLength" $ do
    Tuple ts <- baseTypeOf tuple
    return (length ts)


tupleField :: InsCmp CompileState m => Symbol -> Pointer -> m Pointer
tupleField symbol tuple = trace "tupleField" $ do
    Typedef _ <- return $ typeof tuple
    Tuple ts <- baseTypeOf tuple
    ObjField i <- look symbol
    tupleIdx i tuple


tupleIdx :: InsCmp CompileState m => Int -> Pointer -> m Pointer
tupleIdx i tuple = withErrorPrefix "tuple idx: " $ do
    Tuple ts <- baseTypeOf tuple
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Pointer (ts !! i) <$> gep (loc tuple) [int32 0, int32 $ fromIntegral i]


valTupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
valTupleIdx i tup = do
    Tuple ts <- baseTypeOf tup
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Value (ts !! i) <$> extractValue (op tup) [fromIntegral i]
