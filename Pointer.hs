{-# LANGUAGE FlexibleContexts #-}
module Pointer where

import Data.Maybe
import Data.List
import Control.Monad

import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import Value
import Type
import CompileState
import Monad
import Funcs
import qualified AST as S

valPointerEnum :: InsCmp CompileState m => Value -> m Value
valPointerEnum ptr = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    assert (length ts > 1) "pointer has no enum"
    case ptr of
        Val _ op  -> fmap (Val I64) (extractValue op [0])
        Ptr _ loc -> fmap (Ptr I64) (gep loc [int32 0, int32 0])


valPointerDeref :: InsCmp CompileState m => Value -> m Value
valPointerDeref val = do
    Pointer ts <- assertBaseType isPointer (valType val)
    case ts of
        []  -> error ""
        [t] -> do
            op <- fmap valOp (valLoad val)
            return . (Ptr t) =<< bitcast op =<< fmap LL.ptr (opTypeOf t)
        ts -> error ""


valPointerNull :: InsCmp CompileState m => Type -> m Value
valPointerNull typ = do
    Pointer ts <- assertBaseType isPointer typ
    assert (Void `elem` ts) (show typ ++ " does not support null")

    loc <- valLocal typ
    case ts of
        [t] -> return ()
        ts  -> do
            en <- valPointerEnum loc
            valStore en $ valI64 $ fromJust (elemIndex Void ts)

    valLoad loc



valPointerStore :: InsCmp CompileState m => Value -> Value -> m ()
valPointerStore ptr@(Ptr _ loc) val = do
    Pointer ts  <- assertBaseType isPointer (valType ptr)
    Pointer [t] <- assertBaseType isPointer (valType val)

    assert (t `elem` ts) "incompatible pointer types"
    case ts of
        []  -> error ""
        [_] -> error ""
        ts  -> do
            en <- valPointerEnum ptr
            valStore en $ valI64 $ fromJust (elemIndex t ts)
            valPointerSetPi8 ptr =<< valPointerPi8 val


valPointerPi8 :: InsCmp CompileState m => Value -> m LL.Operand
valPointerPi8 ptr = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    op <- fmap valOp (valLoad ptr)
    case ts of
        []  -> error ""
        [t] -> return op
        _   -> extractValue op [1]


valPointerSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
valPointerSetPi8 ptr@(Ptr _ loc) pi8 = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    case ts of
        []  -> error ""
        [t] -> store loc 0 pi8
        ts  -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


valPointerConstruct :: InsCmp CompileState m => Type -> Value -> m Value
valPointerConstruct typ Null = valPointerNull typ
valPointerConstruct typ val  = do
    Pointer ts  <- assertBaseType isPointer typ
    Pointer ts' <- assertBaseType isPointer (valType val)

    let ts'' = intersect ts ts'
    assert (not $ null ts'') "incompatible pointer types"

    loc <- valLocal typ
    valPointerSetPi8 loc =<< valPointerPi8 val

    case (ts, ts') of
        ([],  _)   -> error ""
        (_,   [])  -> error ""
        ([_], [_]) -> return ()
        (_,   [t]) -> do
            en <- valPointerEnum loc
            valStore en $ valI64 $ fromJust (elemIndex t ts)
        ([t], _)   -> error ""
        (_,   _)   -> do
            valEn <- valLoad =<< valPointerEnum val

            cases <- forM ts'' $ \t -> do
                let b = fmap valOp $ valsInfix S.EqEq valEn $ valI64 $ fromJust (elemIndex t ts')
                let s = do
                    en <- valPointerEnum loc
                    valStore en $ valI64 $ fromJust (elemIndex t ts)
                return (b, s)

            switch_ $ cases ++ [(return (bit 1), void trap)]

    valLoad loc
