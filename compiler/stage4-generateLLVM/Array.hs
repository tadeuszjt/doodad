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


arrayGetElem :: InsCmp CompileState m => Pointer -> Value -> m Pointer
arrayGetElem arr idx = do
    Array n t <- baseTypeOf (typeof arr)
    assertBaseType isIntegral (typeof idx)
    Pointer t <$> gep (loc arr) [int32 0, op idx]

