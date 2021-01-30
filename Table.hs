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
    case tab of
        Ptr _ loc -> fmap (Ptr I64) $ gep loc [int32 0, int32 0]
        Val _ op  -> fmap (Val I64) $ extractValue op [0]


tableCap :: InsCmp CompileState m => Value -> m Value
tableCap tab = do
    Table _ <- assertBaseType isTable (valType tab)
    case tab of
        Ptr _ loc -> fmap (Ptr I64) $ gep loc [int32 0, int32 1]
        Val _ op  -> fmap (Val I64) $ extractValue op [1]


tableRow :: InsCmp CompileState m => Word32 -> Value -> m Value
tableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    assert (fromIntegral i < length ts) "table row index >= num rows"
    let t = ts !! fromIntegral i
    case tab of
        Val _ op  -> fmap (Ptr t) (extractValue op [i+2])
        Ptr _ loc -> do
            pp <- gep loc [int32 0, int32 $ fromIntegral i+2]
            fmap (Ptr t) (load pp 0)


tableSetRow :: InsCmp CompileState m => Value -> Word32 -> Value -> m ()
tableSetRow tab i row = do
    Table ts <- assertBaseType isTable (valType tab)
    checkTypesMatch (valType row) (ts !! fromIntegral i)
    pp <- gep (valLoc tab) [int32 0, int32 $ fromIntegral i+2]
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
    len <- valLoad =<< tableLen tab
    cap <- valLoad =<< tableCap tab

    baseStart <- assertBaseType isInt (valType start)
    baseEnd   <- assertBaseType isInt (valType end)

    bStartNeg <- valsInfix S.LT start (valI64 0) 
    startLoc <- valLocal (valType start)
    if_ (valOp bStartNeg)
        (valStore startLoc (valI64 0))
        (valStore startLoc start)

    bEndGT <- valsInfix S.GTEq end len
    endLoc <- valLocal (valType end)
    if_ (valOp bEndGT)
        (valStore endLoc len)
        (valStore endLoc end)

    loc <- valLocal (valType tab)
    locLen <- tableLen loc
    locCap <- tableCap loc
    valStore locLen =<< valsInfix S.Minus endLoc startLoc
    valStore locCap =<< valsInfix S.Minus cap startLoc

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab 
        tableSetRow loc i =<< valPtrIdx row startLoc

    valLoad loc



tableAppend :: InsCmp CompileState m => Value -> Value -> m Value
tableAppend tab tup = do
    Table ts  <- assertBaseType isTable (valType tab)
    Tuple ts' <- assertBaseType isTuple (valType tup)

    -- check types match
    assert (length ts == length ts') "tuple type does not match table column"
    zipWithM_ checkTypesMatch ts ts'

    -- create local table
    loc <- valLocal (valType tab)
    valStore loc tab

    cap <- tableCap loc
    len <- tableLen loc

    capZero <- valsInfix S.LTEq cap (valI64 0)
    lenZero <- valsInfix S.LTEq len (valI64 0)
    empty   <- valsInfix S.AndAnd lenZero capZero
    full    <- valsInfix S.LTEq cap len

    let emptyCase = do
        valStore cap (valI64 16)
        forM_ (zip ts [0..]) $ \(t, i) ->
            tableSetRow loc i =<< valMalloc t cap
    
    let fullCase = do
        valStore cap =<< valsInfix S.Times len (valI64 2)
        forM_ (zip ts [0..]) $ \(t, i) -> do
            mal <- valMalloc t cap
            row <- tableRow i loc
            valMemCpy mal row len
            tableSetRow loc i mal

    switch_ [
        (return (valOp empty), emptyCase),
        (return (valOp full), fullCase)
        ]

    tableSetElem loc len tup
    valStore len =<< valsInfix S.Plus len (valI64 1)
    return loc
