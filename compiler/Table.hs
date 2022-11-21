{-# LANGUAGE FlexibleContexts #-}
module Table where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST.Instruction

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
import Symbol

tableTypeDef :: InsCmp CompileState m => Symbol -> S.AnnoType -> m ()
tableTypeDef symbol (S.AnnoType typ) = trace "tableTypeDef" $ do
    base@(Table ts) <- assertBaseType isTable typ
    name <- addTypeDef symbol =<< opTypeOf base
    let typdef = Typedef symbol

--    define symbol (KeyFunc [] typdef) ObjConstructor
--    define symbol (KeyFunc [typ] typdef) ObjConstructor
--    define symbol (KeyFunc [typdef] typdef) ObjConstructor
    define symbol KeyType $ ObType typ (Just name)

--    when (length ts > 0) $
--        define symbol (KeyFunc ts typdef) ObjConstructor
--


tableLen :: InsCmp CompileState m => Value -> m Value
tableLen tab = trace "tableLen" $ do
    Table _ <- assertBaseType isTable (valType tab)
    case tab of
        Val _ op -> Val I64 <$> extractValue op [0]
        Ptr _ loc -> valLoad =<< Ptr I64 <$> gep loc [int32 0, int32 0]


tableCap :: InsCmp CompileState m => Value -> m Value
tableCap tab = trace "tableLen" $ do
    Table _ <- assertBaseType isTable (valType tab)
    case tab of
        Val _ op -> Val I64 <$> extractValue op [1]
        Ptr _ loc -> valLoad =<< Ptr I64 <$> gep loc [int32 0, int32 1]


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
        valStore siz =<< valIntInfix S.Plus siz =<< valIntInfix S.Times len =<< sizeOf t
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
        Val _ op  -> do
            retty <- LL.ptr <$> opTypeOf (ts !! i)
            r <- emitInstr retty $ ExtractValue op [fromIntegral i + 2] []
            return $ Ptr (ts !! i) r

        Ptr _ loc -> do
            r <- gep loc [int32 0, int32 (fromIntegral i + 2)]
            Ptr (ts !! i) <$> load r 0
    

tableSetRow :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (valType row == ts !! i) "Types do not match"
    pp <- gep (valLoc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (valLoc row)


tableGetElem :: InsCmp CompileState m => Value -> Value -> m [Value]
tableGetElem tab idx = trace "tableGetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        valPtrIdx row idx


tableSetRowElem :: InsCmp CompileState m => Value -> Int -> Value -> Value -> m ()
tableSetRowElem tab row col val = trace "tableSetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (row >= 0 && row < length ts) "row out of range"
    let t = ts !! row
    assertBaseType (== t) (valType val)
    assertBaseType (== I64) (valType col)

    rowPtr <- tableRow row tab
    ptr <- valPtrIdx rowPtr col
    valStore ptr val


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

    startLT0 <- valIntInfix S.LT startArg (valI64 0)
    valStore start =<< valSelect startLT0 (valI64 0) startArg

    endGT <- valIntInfix S.GT endArg len
    valStore end =<< valSelect endGT len endArg

    startGT <- valIntInfix S.GT start len
    valStore start =<< valSelect startGT len start

    crossed <- valIntInfix S.GT start end
    valStore end =<< valSelect crossed start end
    
    loc <- valLocal (valType tab)
    tableSetLen loc =<< valIntInfix S.Minus end start
    tableSetCap loc =<< valIntInfix S.Minus cap start

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab 
        tableSetRow loc i =<< valPtrIdx row start

    return loc


tableGrow :: InsCmp CompileState m => Value -> Value -> m ()
tableGrow tab newLen = trace "tableGrow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType newLen)

    bFull <- valIntInfix S.GT newLen =<< tableCap tab
    if_ (valOp bFull) (fullCase ts) (return ())
    tableSetLen tab newLen

    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            newTab <- tableMake (valType tab) =<< valIntInfix S.Times newLen (valI64 2)
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
    tableGrow tab =<< valIntInfix S.Plus len (valI64 1)
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
    newLen <- valIntInfix S.Plus locLen valLen
    tableGrow loc newLen

    -- copy b into loc
    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i loc
        dst <- valPtrIdx row locLen 
        src <- tableRow i val
        valMemCpy dst src valLen


tablePopElem :: InsCmp CompileState m => Value -> m [Value]
tablePopElem tab = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- tableLen tab
    newLen <- valIntInfix S.Minus len =<< valInt (valType len) 1
    tableSetLen tab newLen
    tableGetElem tab newLen


tableClear :: InsCmp CompileState m => Value -> m ()
tableClear tab = do
    Table ts <- assertBaseType isTable (valType tab)
    tableSetLen tab (valI64 0)



