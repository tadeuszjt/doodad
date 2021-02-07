{-# LANGUAGE FlexibleContexts #-}
module ADT where

import Data.Maybe
import Data.List
import Control.Monad

import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST as S
import Value
import Type
import CompileState
import Monad
import Funcs


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum ptr = do
    ADT ts <- assertBaseType isADT (valType ptr)
    assert (length ts > 1) "adt has no enum"
    case ptr of
        Val _ op  -> fmap (Val I64) (extractValue op [0])
        Ptr _ loc -> fmap (Ptr I64) (gep loc [int32 0, int32 0])



adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum ptr@(Ptr _ loc) i = do
    ADT ts <- assertBaseType isADT (valType ptr)
    assert (length ts > 1)           "adt type has no enum"
    assert (i >= 0 && i < length ts) "invalid adt enum"

    en <- fmap (Ptr I64) (gep loc [int32 0, int32 0])
    valStore en (valI64 i)


adtDeref :: InsCmp CompileState m => Value -> m Value
adtDeref val = do
    ADT ts <- assertBaseType isADT (valType val)
    assert (length ts == 1) "cannot dereference multi-type adt"
    let [t] = ts
    pi8 <- adtPi8 val
    pt  <- fmap LL.ptr (opTypeOf t)
    fmap (Ptr t) (bitcast pi8 pt)


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull typ = do
    ADT ts <- assertBaseType isADT typ
    let ns = filter (== Void) ts
    assert (length ns == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length ts > 1) $
        adtSetEnum loc $ fromJust (elemIndex Void ts)

    valLoad loc


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 ptr = do
    ADT ts <- assertBaseType isADT (valType ptr)
    op <- fmap valOp (valLoad ptr)
    case ts of
        []  -> return op
        [t] -> return op
        _   -> extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 ptr@(Ptr _ loc) pi8 = do
    ADT ts <- assertBaseType isADT (valType ptr)
    case ts of
        []  -> error ""
        [t] -> store loc 0 pi8
        ts  -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


adtConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
adtConstructField sym typ [val] = do
    ADT ts <- assertBaseType isADT typ

    loc <- valLocal typ
    let tn = Named sym (valType val)
    assert (tn `elem` ts) "invalid adt field constructor"

    loc <- valLocal typ
    adtSetEnum loc $ fromJust (elemIndex tn ts)

    mal <- valMalloc (valType val) (valI64 1)
    valStore mal val
    adtSetPi8 loc =<< bitcast (valLoc mal) (LL.ptr LL.i8)
    valLoad loc


adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct typ Null = adtNull typ
adtConstruct typ val  = do
    ADT ts  <- assertBaseType isADT typ

    let tn = valType val
    assert (tn `elem` ts) "invalid adt constructor"

    loc <- valLocal typ
    adtSetEnum loc $ fromJust (elemIndex tn ts)
    mal <- valMalloc (valType val) (valI64 1)
    valStore mal val
    adtSetPi8 loc =<< bitcast (valLoc mal) (LL.ptr LL.i8)
    valLoad loc
