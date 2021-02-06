{-# LANGUAGE FlexibleContexts #-}
module Table where

import Data.Word
import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST as S
import Monad
import Value
import CompileState
import Funcs
import Type 



tableLen :: InsCmp CompileState m => Value -> m Value
tableLen tab = do
    Table _ <- assertBaseType isTable (valType tab)
    op <- fmap valOp (valLoad tab)
    fmap (Val I64) (extractValue op [0])


tableCap :: InsCmp CompileState m => Value -> m Value
tableCap tab = do
    Table _ <- assertBaseType isTable (valType tab)
    op <- fmap valOp (valLoad tab)
    fmap (Val I64) (extractValue op [1])


tableSetLen :: InsCmp CompileState m => Value -> Value -> m ()
tableSetLen tab@(Ptr _ loc) len = do
    Table _ <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType len)
    l <- gep loc [int32 0, int32 0]
    store l 0 =<< fmap valOp (valLoad len)


tableSetCap :: InsCmp CompileState m => Value -> Value -> m ()
tableSetCap tab@(Ptr _ loc) cap = do
    Table _ <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType cap)
    c <- gep loc [int32 0, int32 1]
    store c 0 =<< fmap valOp (valLoad cap)


tableRow :: InsCmp CompileState m => Word32 -> Value -> m Value
tableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    let t = ts !! fromIntegral i
    op <- fmap valOp (valLoad tab)
    fmap (Ptr t) (extractValue op [i+2])


tableSetRow :: InsCmp CompileState m => Value -> Word32 -> Value -> m ()
tableSetRow tab i row = do
    Table ts <- assertBaseType isTable (valType tab)
    checkTypesMatch (valType row) (ts !! fromIntegral i)
    pp <- gep (valLoc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (valLoc row)


tableGetElem :: InsCmp CompileState m => Value -> Value -> m Value
tableGetElem tab idx = do
    Table ts <- assertBaseType isTable (valType tab)

    tup <- valLocal (Tuple ts)
    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        ptr <- valPtrIdx row idx
        valTupleSet tup (fromIntegral i) ptr

    return tup


tableSetElem :: InsCmp CompileState m => Value -> Value -> Value -> m ()
tableSetElem tab idx tup = do
    Table ts  <- assertBaseType isTable (valType tab)
    Tuple ts' <- assertBaseType isTuple (valType tup)
    idxType   <- assertBaseType isInt (valType idx)

    -- check types match
    assert (length ts == length ts') "tuple type does not match table column"
    zipWithM_ checkTypesMatch ts ts'

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        ptr <- valPtrIdx row idx
        valStore ptr =<< valTupleIdx tup (fromIntegral i)


tableRange :: InsCmp CompileState m => Value -> Value -> Value -> m Value
tableRange tab start end = do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType start)
    assertBaseType isInt (valType end)

    cap <- tableCap tab
    
    loc <- valLocal (valType tab)
    tableSetLen loc =<< valsInfix S.Minus end start
    tableSetCap loc =<< valsInfix S.Minus cap start

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab 
        tableSetRow loc i =<< valPtrIdx row start

    return loc


tableAppend :: InsCmp CompileState m => Value -> Value -> m Value
tableAppend a b = do
    Table ts <- assertBaseType isTable (valType a)
    assertBaseType isTable (valType b)

    ap <- pureTypeOf (valType a)
    bp <- pureTypeOf (valType b)
    checkTypesMatch ap bp

    loc <- valLocal (valType a)
    valStore loc a

    aLen <- tableLen a
    bLen <- tableLen b
    newLen <- valsInfix S.Plus aLen bLen
    tableSetLen loc newLen

    --increase cap
    let fullCase = do
        tableSetCap loc =<< valsInfix S.Times newLen (valI64 2)
        forM_ (zip ts [0..]) $ \(t, i) -> do
            tableSetRow loc i =<< valMalloc t =<< tableCap loc
            dst <- tableRow i loc
            src <- tableRow i a
            valMemCpy dst src aLen

    bFull <- valsInfix S.GT newLen =<< tableCap loc
    if_ (valOp bFull) fullCase (return ())

    -- copy b into loc
    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i loc
        dst <- valPtrIdx row aLen 
        src <- tableRow i b
        valMemCpy dst src bLen

    return loc
