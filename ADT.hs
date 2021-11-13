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
    ADT xs <- assertBaseType isADT (valType ptr)
    assert (length xs > 1) "adt has no enum"
    case ptr of
        Val _ op  -> Val I64 <$> extractValue op [0]
        Ptr _ loc -> Ptr I64 <$> gep loc [int32 0, int32 0]



adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum ptr@(Ptr _ loc) i = do
    ADT xs <- assertBaseType isADT (valType ptr)
    assert (length xs > 1)           "adt type has no enum"
    assert (i >= 0 && i < length xs) "invalid adt enum"

    en <- Ptr I64 <$> gep loc [int32 0, int32 0]
    valStore en (valI64 i)


adtDeref :: InsCmp CompileState m => Value -> m Value
adtDeref val = do
    ADT xs <- assertBaseType isADT (valType val)
    assert (length xs == 1) "cannot dereference multi-type adt"
    let [(s, t)] = xs
    pi8 <- adtPi8 val
    pt  <- LL.ptr <$> opTypeOf t
    Ptr t <$> bitcast pi8 pt


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull typ = do
    ADT xs <- assertBaseType isADT typ
    let is = [ i | (("", Void), i) <- zip xs [0..] ]
    assert (length is == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length xs > 1) $ adtSetEnum loc (head is)
    valLoad loc


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 ptr = do
    ADT ts <- assertBaseType isADT (valType ptr)
    op <- valOp <$> valLoad ptr
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


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
adtConstructField sym adtTyp [val] = do
    ADT xs <- assertBaseType isADT adtTyp

    adt <- valLocal adtTyp
    let xn = (sym, valType val)
    assert (xn `elem` xs) ("invalid adt field constructor for: " ++ sym)

    adtSetEnum adt $ fromJust (elemIndex xn xs)

    mal <- valMalloc (valType val) (valI64 1)
    valStore mal val
    adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
    return adt


adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct adtTyp (Exp (S.Null _)) = adtNull adtTyp
adtConstruct adtTyp val              = do
    ADT xs  <- assertBaseType isADT adtTyp

    let is = [ i | ((s, t), i) <- zip xs [0..], t == valType val ]
    assert (length is == 1) "cannot resolve adt from type"
    let i = head is

    loc <- valLocal adtTyp
    adtSetEnum loc i
    mal <- valMalloc (valType val) (valI64 1)
    valStore mal val
    adtSetPi8 loc =<< bitcast (valLoc mal) (LL.ptr LL.i8)
    valLoad loc
