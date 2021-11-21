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
import State
import Monad
import Funcs


-- ADT type rules:
-- ADT []         *i8
-- ADT [(s, f64)] *f64
-- ADT [(s, f64), (t, i64)] struct { enum type, i8* }


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = do
    ADT xs <- assertBaseType isADT (valType adt)
    assert (length xs > 1) "adt has no enum"
    case adt of
        Val _ op  -> Val I64 <$> extractValue op [0]
        Ptr _ loc -> Ptr I64 <$> gep loc [int32 0, int32 0]



adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt@(Ptr _ loc) i = do
    ADT xs <- assertBaseType isADT (valType adt)
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
    return loc


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 adt = do
    ADT xs <- assertBaseType isADT (valType adt)
    op <- valOp <$> valLoad adt
    case xs of
        []  -> error ""
        [x] -> return op
        xs  -> extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 adt@(Ptr _ loc) pi8 = do
    ADT xs <- assertBaseType isADT (valType adt)
    case xs of
        []  -> error ""
        [x] -> store loc 0 pi8
        xs  -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
adtConstructField sym adtTyp [val] = do
    ADT xs <- assertBaseType isADT adtTyp

    adt <- valLocal adtTyp
    let xn = (sym, valType val)
    assert (xn `elem` xs) ("invalid adt field constructor for: " ++ sym)

    when (length xs > 1) $ adtSetEnum adt $ fromJust (elemIndex xn xs)

    mal <- valMalloc (valType val) (valI64 1)
    valStore mal val
    adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
    return adt


-- ADT()       -> zero constructor
-- ADT(i64(n)) -> construct from unique type field
-- ADT(adt2)   -> construct from adt with ONE equivalent field
-- ADT(null)   -> null becomes adt with equivalent field
adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct adtTyp (Exp (S.Null _)) = adtNull adtTyp
adtConstruct adtTyp (Exp _)          = error "adt constructing from contextual"
adtConstruct adtTyp val              = do
    ADT xs  <- assertBaseType isADT adtTyp

    let is = [ i | ((s, t), i) <- zip xs [0..], t == valType val, s == "" ]
    if length is == 1 then do -- has unique type field
        let i = head is
        adt <- valLocal adtTyp
        adtSetEnum adt i
        mal <- valMalloc (valType val) (valI64 1)
        valStore mal val
        adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
        return adt
    else do                   -- must be another adt
        ADT xs' <- assertBaseType isADT (valType val)
        assert (length xs' == 1) "Argument ADT must have one field"
        assert (head xs' `elem` xs) "Argument ADT does have equivalent field"
        let x@(s, t) = head xs'

        adt <- valLocal adtTyp
        adtSetEnum adt $ fromJust (elemIndex x xs)

        if t == Void then return adt
        else do
            mal <- valMalloc t (valI64 1)
            valStore mal =<< adtDeref val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt

