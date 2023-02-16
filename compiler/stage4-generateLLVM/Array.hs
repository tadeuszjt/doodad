{-# LANGUAGE FlexibleContexts #-}
module Array where

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import Type
import State
import Monad
import Typeof
import Value


ptrArrayGetElemConst :: (InsCmp CompileState m, Integral n) => Value -> n -> m Value
ptrArrayGetElemConst arr idx = do
    Array n t <- assertBaseType isArray (typeof arr)
    Ptr t <$> gep (valLoc arr) [int32 0, int32 $ fromIntegral idx]


ptrArrayGetElem :: InsCmp CompileState m => Value -> Value -> m Value
ptrArrayGetElem arr idx = do
    Array n t <- assertBaseType isArray (typeof arr)
    assertBaseType isIntegral (typeof idx)
    idxOp <- valOp <$> valLoad idx
    Ptr t <$> gep (valLoc arr) [int32 0, idxOp]

