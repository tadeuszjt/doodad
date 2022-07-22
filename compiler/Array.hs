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

arrayMake :: InsCmp CompileState m => Type -> m Value
arrayMake typ = do
    Array n t <- assertBaseType isArray typ
    valLocal typ


arrayGetElemConst :: (InsCmp CompileState m, Integral n) => Value -> n -> m Value
arrayGetElemConst arr idx = do
    Array n t <- assertBaseType isArray (valType arr)
    case arr of
        Ptr _ loc -> Ptr t <$> gep loc [int32 0, int32 $ fromIntegral idx]
        Val _ op  -> Val t <$> extractValue op [fromIntegral idx]

arrayGetElem :: InsCmp CompileState m => Value -> Value -> m Value
arrayGetElem arr idx = do
    Array n t <- assertBaseType isArray (valType arr)
    assertBaseType isIntegral (valType idx)
    idxOp <- valOp <$> valLoad idx
    case arr of
        Ptr _ loc -> Ptr t <$> gep loc [int32 0, idxOp]
        Val _ op  -> fail "can't get element of value array"

arraySetElem :: InsCmp CompileState m => Value -> Value -> Value -> m ()
arraySetElem arr idx elm = do
    Array n t <- assertBaseType isArray (valType arr)
    fail "benis"

