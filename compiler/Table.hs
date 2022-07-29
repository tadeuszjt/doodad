{-# LANGUAGE FlexibleContexts #-}
module Table where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST as S
import Monad
import Value
import State
import Funcs
import Type 
import Tuple
import Typeof
import Trace
import Error



tableLen :: InsCmp CompileState m => Value -> m Value
tableLen tab = trace "tableLen" $ do
    Table _ <- assertBaseType isTable (valType tab)
    op <- valOp <$> valLoad tab
    Val I64 <$> extractValue op [0]


tableCap :: InsCmp CompileState m => Value -> m Value
tableCap tab = trace "tableCap" $ do
    Table _ <- assertBaseType isTable (valType tab)
    op <- valOp <$> valLoad tab
    Val I64 <$> extractValue op [1]


tableSetLen :: InsCmp CompileState m => Value -> Value -> m ()
tableSetLen tab@(Ptr _ loc) len = trace "tableSetLen" $ do
    Table _ <- assertBaseType isTable (valType tab)
    assertBaseType (==I64) (valType len)
    l <- gep loc [int32 0, int32 0]
    store l 0 . valOp =<< valLoad len


tableSetCap :: InsCmp CompileState m => Value -> Value -> m ()
tableSetCap tab@(Ptr _ loc) cap = trace "tableSetCap" $ do
    Table _ <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType cap)
    c <- gep loc [int32 0, int32 1]
    store c 0 . valOp =<< valLoad cap


tableMake :: InsCmp CompileState m => Type -> Value -> m Value
tableMake typ len = trace "tableMake" $ do
    Table ts <- assertBaseType isTable typ
    assertBaseType (==I64) (valType len)
    
    tab <- valLocal typ
    tableSetLen tab len
    tableSetCap tab len

    siz <- valLocal I64
    valStore siz (valI64 0)
    idxs <- forM ts $ \t -> do
        idx <- valLoad siz
        valStore siz =<< valsInfix S.Plus siz =<< valsInfix S.Times len =<< sizeOf t
        return idx

    mal <- valMalloc I8 siz

    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        ptr <- valPtrIdx mal idx
        ptr' <- fmap (Ptr t) $ bitcast (valLoc ptr) =<< fmap LL.ptr (opTypeOf t)
        tableSetRow tab i ptr'

    return tab



tableRow :: InsCmp CompileState m => Int -> Value -> m Value
tableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    assert (i >= 0 && i < length ts) "Invalid table row index"
    case tab of
        Val _ op  -> Ptr (ts !! i) <$> extractValue op [fromIntegral i + 2]
        Ptr _ loc -> do
            r <- gep loc [int32 0, int32 (fromIntegral i + 2)]
            Ptr (ts !! i) <$> load r 0
    

tableSetRow :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (valType row == ts !! i) "Types do not match"
    pp <- gep (valLoc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (valLoc row)


tableGetElem :: InsCmp CompileState m => Value -> Value -> m Value
tableGetElem tab idx = trace "tableGetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    case ts of
        [t] -> do
            row <- tableRow 0 tab
            valPtrIdx row idx
        ts  -> do
            tup <- valLocal (Tuple ts)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                row <- tableRow i tab
                tupleSet tup i =<< valPtrIdx row idx

            return tup


tableSetElem :: InsCmp CompileState m => Value -> Value -> Value -> m ()
tableSetElem tab idx tup = trace "tableSetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    Tuple tts <- assertBaseType isTuple (valType tup)
    idxType  <- assertBaseType isInt (valType idx)
    assert (ts == tts) "tuple type does not match table column"

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        ptr <- valPtrIdx row idx
        valStore ptr =<< tupleIdx i tup


tableRange :: InsCmp CompileState m => Value -> Value -> Value -> m Value
tableRange tab startArg endArg = trace "tableRange" $ do
    Table ts <- baseTypeOf (valType tab)

    assertBaseType isInt (valType startArg)
    assertBaseType isInt (valType endArg)

    len <- tableLen tab
    cap <- tableCap tab

    start <- valLocal (valType startArg)
    end   <- valLocal (valType endArg)

    startLT0 <- valsInfix S.LT startArg (valI64 0)
    valStore start =<< valSelect startLT0 (valI64 0) startArg

    endGT <- valsInfix S.GT endArg len
    valStore end =<< valSelect endGT len endArg

    startGT <- valsInfix S.GT start len
    valStore start =<< valSelect startGT len start

    crossed <- valsInfix S.GT start end
    valStore end =<< valSelect crossed start end
    
    loc <- valLocal (valType tab)
    tableSetLen loc =<< valsInfix S.Minus end start
    tableSetCap loc =<< valsInfix S.Minus cap start

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab 
        tableSetRow loc i =<< valPtrIdx row start

    return loc


tableGrow :: InsCmp CompileState m => Value -> Value -> m ()
tableGrow tab newLen = trace "tableGrow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType newLen)

    bFull <- valsInfix S.GT newLen =<< tableCap tab
    if_ (valOp bFull) (fullCase ts) (return ())
    tableSetLen tab newLen

    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            newTab <- tableMake (valType tab) =<< valsInfix S.Times newLen (valI64 2)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                newRow <- tableRow i newTab
                oldRow <- tableRow i tab
                valMemCpy newRow oldRow =<< tableLen tab

            valStore tab newTab


tableAppendElem :: InsCmp CompileState m => Value -> Value -> m ()
tableAppendElem tab val = trace "tableAppendElem" $ do
    Table [t] <- assertBaseType isTable (valType tab)
    assert (valType val == t) "Types do not match."
    
    len <- tableLen tab
    tableGrow tab =<< valsInfix S.Plus len (valI64 1)
    row <- tableRow 0 tab
    ptr <- valPtrIdx row len
    valStore ptr val


tableAppend :: InsCmp CompileState m => Value -> Value -> m ()
tableAppend loc val = trace "tableAppend" $ do
    Table ts <- assertBaseType isTable (valType loc)
    assertBaseType isTable (valType val)
    assert (valType loc == valType val) "Types do not match."

    locLen <- tableLen loc
    valLen <- tableLen val
    newLen <- valsInfix S.Plus locLen valLen
    tableGrow loc newLen

    -- copy b into loc
    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i loc
        dst <- valPtrIdx row locLen 
        src <- tableRow i val
        valMemCpy dst src valLen
