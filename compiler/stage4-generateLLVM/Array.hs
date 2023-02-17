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


arrayGetElem :: InsCmp CompileState m => Pointer -> Value2 -> m Pointer
arrayGetElem arr idx = do
    Array n t <- baseTypeOf (typeof arr)
    assertBaseType isIntegral (typeof idx)
    Pointer t <$> gep (loc arr) [int32 0, op idx]

