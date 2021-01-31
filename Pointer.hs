{-# LANGUAGE FlexibleContexts #-}
module Pointer where

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


pointerEnum :: InsCmp CompileState m => Value -> m Value
pointerEnum ptr = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    assert (length ts > 1) "pointer has no enum"
    case ptr of
        Val _ op  -> fmap (Val I64) (extractValue op [0])
        Ptr _ loc -> fmap (Ptr I64) (gep loc [int32 0, int32 0])



pointerSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
pointerSetEnum ptr@(Ptr _ loc) i = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    assert (length ts > 1)           "pointer type has no enum"
    assert (i >= 0 && i < length ts) "invalid pointer enum"

    en <- fmap (Ptr I64) (gep loc [int32 0, int32 0])
    valStore en (valI64 i)


pointerDeref :: InsCmp CompileState m => Value -> m Value
pointerDeref val = do
    Pointer ts <- assertBaseType isPointer (valType val)
    assert (length ts == 1) "cannot dereference multi-type pointer"
    let [t] = ts
    pi8 <- pointerPi8 val
    pt  <- fmap LL.ptr (opTypeOf t)
    fmap (Ptr t) (bitcast pi8 pt)


pointerNull :: InsCmp CompileState m => Type -> m Value
pointerNull typ = do
    Pointer ts <- assertBaseType isPointer typ
    let ns = filter (== Void) ts
    assert (length ns == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length ts > 1) $
        pointerSetEnum loc $ fromJust (elemIndex Void ts)

    valLoad loc


pointerPi8 :: InsCmp CompileState m => Value -> m LL.Operand
pointerPi8 ptr = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    op <- fmap valOp (valLoad ptr)
    case ts of
        []  -> return op
        [t] -> return op
        _   -> extractValue op [1]


pointerSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
pointerSetPi8 ptr@(Ptr _ loc) pi8 = do
    Pointer ts <- assertBaseType isPointer (valType ptr)
    case ts of
        []  -> error ""
        [t] -> store loc 0 pi8
        ts  -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


pointerConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
pointerConstructField sym typ [val] = do
    Pointer ts <- assertBaseType isPointer typ
    Pointer [t] <- assertBaseType isPointer (valType val)
    let tn = Named sym t
    assert (tn `elem` ts) "invalid pointer field constructor"

    loc <- valLocal typ
    pointerSetEnum loc $ fromJust (elemIndex tn ts)
    pointerSetPi8 loc =<< pointerPi8 val

    valLoad loc


--pointerMatchPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
--pointerMatchPattern pat val = do
--    Pointer ts <- assertBaseType isPointer (valType val)
--
--    case pat of
--        S.PatLiteral S.Null -> do
            
            




pointerConstruct :: InsCmp CompileState m => Type -> Value -> m Value
pointerConstruct typ Null = pointerNull typ
pointerConstruct typ val  = do
    Pointer ts  <- assertBaseType isPointer typ
    Pointer ts' <- assertBaseType isPointer (valType val)

    assert (not $ null $ intersect ts ts') "incompatible pointer types"

    loc <- valLocal typ
    pointerSetPi8 loc =<< pointerPi8 val

    case (ts, ts') of
        ([],  _)   -> error ""
        (_,   [])  -> error ""
        ([_], [_]) -> return ()
        (_,   [t]) -> do
            en <- pointerEnum loc
            valStore en $ valI64 $ fromJust (elemIndex t ts)
        ([t], _)   -> do
            valEn <- valLoad =<< pointerEnum val
            b <- valsInfix S.EqEq valEn $ valI64 $ fromJust (elemIndex t ts')
            if_ (valOp b) (return ()) (void trap)

        (_,   _)   -> do
            valEn <- pointerEnum val

            cases <- forM (intersect ts ts') $ \t -> do
                let b = fmap valOp $ valsInfix S.EqEq valEn $ valI64 $ fromJust (elemIndex t ts')
                let s = pointerSetEnum loc $ fromJust (elemIndex t ts)
                return (b, s)

            switch_ $ cases ++ [(return (bit 1), void trap)]

    valLoad loc
