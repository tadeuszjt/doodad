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
    define symbol KeyType $ ObType typ (Just name)


mkTableLen :: InsCmp CompileState m => Value -> m Value
mkTableLen tab = do
    Table _ <- assertBaseType isTable (valType tab)
    Val I64 <$> case tab of
        Val _ op  -> extractValue op [0]
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 0]


mkTableCap :: InsCmp CompileState m => Value -> m Value
mkTableCap tab = do
    Table _ <- assertBaseType isTable (valType tab)
    Val I64 <$> case tab of
        Val _ op  -> extractValue op [1]
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 1]


-- allocate a table with the specified type and length
mkTable :: InsCmp CompileState m => Type -> Value -> m Value
mkTable typ len = do
    Table ts <- assertBaseType isTable typ
    assertBaseType (==I64) (valType len)
    
    tab <- mkAlloca typ
    tableSetLen tab len
    tableSetCap tab len

    siz <- mkAlloca I64
    valStore siz (mkI64 0)
    idxs <- forM ts $ \t -> do
        idx <- valLoad siz
        valStore siz =<< mkIntInfix S.Plus siz =<< mkIntInfix S.Times len =<< sizeOf t
        return idx

    mal <- mkMalloc I8 siz
    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        Ptr _ pi8 <- ptrIdx mal idx
        ptr <- fmap (Ptr t) $ bitcast pi8 =<< LL.ptr <$> opTypeOf t
        tableSetRow tab i ptr

    return tab


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



tableRow :: InsCmp CompileState m => Int -> Value -> m Value
tableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    assert (i >= 0 && i < length ts) "Invalid table row index"
    case tab of
        Val _ op  -> do
            pType <- LL.ptr <$> opTypeOf (ts !! i)
            ptr <- emitInstr pType $ ExtractValue op [fromIntegral i + 2] []
            return $ Ptr (ts !! i) ptr
        Ptr _ loc -> do
            pptr <- gep loc [int32 0, int32 (fromIntegral i + 2)]
            Ptr (ts !! i) <$> load pptr 0
    

tableSetRow :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (valType row == ts !! i) "Types do not match"
    pp <- gep (valLoc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (valLoc row)


ptrsTableColumn :: InsCmp CompileState m => Value -> Value -> m [Value]
ptrsTableColumn tab idx = trace "tableGetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        ptrIdx row idx


tableSetColumn :: InsCmp CompileState m => Value -> Value -> [Value] -> m ()
tableSetColumn tab idx vals = trace "tableSetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (ts == map valType vals) "Elem types incorrect"
    idxType  <- assertBaseType isInt (valType idx)

    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        ptr <- ptrIdx row idx
        valStore ptr (vals !! i)


tableGrow :: InsCmp CompileState m => Value -> Value -> m ()
tableGrow tab newLen = trace "tableGrow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType newLen)

    bFull <- mkIntInfix S.GT newLen =<< mkTableCap tab
    if_ (valOp bFull) (fullCase ts) (return ())
    tableSetLen tab newLen

    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            newTab <- mkTable (valType tab) =<< mkIntInfix S.Times newLen (mkI64 2)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                newRow <- tableRow i newTab
                oldRow <- tableRow i tab
                valMemCpy newRow oldRow =<< mkTableLen tab

            valStore tab newTab


tableAppendColumn :: InsCmp CompileState m => Value -> [Value] -> m ()
tableAppendColumn tab vals = trace "tableAppendElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (map valType vals == ts) $ "Types do not match."
    
    len <- mkTableLen tab
    tableGrow tab =<< mkIntInfix S.Plus len (mkI64 1)
    forM_ (zip vals [0..]) $ \(v, i) -> do
        row <- tableRow i tab
        ptr <- ptrIdx row len
        valStore ptr v


tableAppend :: InsCmp CompileState m => Value -> Value -> m ()
tableAppend loc val = trace "tableAppend" $ do
    Table ts <- assertBaseType isTable (valType loc)
    assertBaseType isTable (valType val)
    assert (valType loc == valType val) "Types do not match."

    locLen <- mkTableLen loc
    valLen <- mkTableLen val
    newLen <- mkIntInfix S.Plus locLen valLen
    tableGrow loc newLen

    -- copy b into loc
    forM_ (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i loc
        dst <- ptrIdx row locLen 
        src <- tableRow i val
        valMemCpy dst src valLen


mkTablePop :: InsCmp CompileState m => Value -> m [Value]
mkTablePop tab = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- mkTableLen tab
    newLen <- mkIntInfix S.Minus len (mkI64 1)
    tableSetLen tab newLen
    mapM valLoad =<< ptrsTableColumn tab newLen


tableClear :: InsCmp CompileState m => Value -> m ()
tableClear tab = do
    Table ts <- assertBaseType isTable (valType tab)
    tableSetLen tab (mkI64 0)

