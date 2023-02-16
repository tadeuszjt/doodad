{-# LANGUAGE FlexibleContexts #-}
module Table where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST.Instruction

import qualified AST
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


tableLen :: InsCmp CompileState m => Pointer -> m Pointer
tableLen table = do
    Table _ <- baseTypeOf (typeof table)
    Pointer I64 <$> gep (loc table) [int32 0, int32 0]


tableCap :: InsCmp CompileState m => Pointer -> m Pointer
tableCap table = do
    Table _ <- baseTypeOf (typeof table)
    Pointer I64 <$> gep (loc table) [int32 0, int32 1]


-- allocate a table with the specified type and length
mkTable :: InsCmp CompileState m => Type -> Value -> m Value
mkTable typ initialLen = do
    Table ts <- assertBaseType isTable typ
    assertBaseType (==I64) (valType initialLen)

    tab <- newVal typ
    cap <- tableCap tab
    len <- tableLen tab
    valStore (fromPointer cap) initialLen
    valStore (fromPointer len) initialLen

    siz <- newI64 0
    idxs <- forM ts $ \t -> do
        idx <- valLoad (Ptr I64 $ loc siz)
        Val I64 op <- mkIntInfix AST.Plus (Ptr I64 $ loc siz) =<< mkIntInfix AST.Times initialLen =<< sizeOf t
        store (loc siz) 0 op
        return idx

    mal <- mkMalloc I8 (Ptr I64 $ loc siz)
    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        Pointer _ pi8 <- advancePointer (toPointer mal) idx
        ptr <- fmap (Pointer t) $ bitcast pi8 =<< LL.ptr <$> opTypeOf t
        tableSetRow tab i ptr

    return (fromPointer tab)


tableColumn :: InsCmp CompileState m => Pointer -> Value -> m [Pointer]
tableColumn tab idx = trace "tableGetElem" $ do
    Table ts <- baseTypeOf (typeof tab)
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i (fromPointer tab)
        advancePointer row idx



tableRow :: InsCmp CompileState m => Int -> Value -> m Pointer
tableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    assert (i >= 0 && i < length ts) "Invalid table row index"
    Pointer (ts !! i) <$> case tab of
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 (fromIntegral i + 2)]
        Val _ op  -> extractValue op [fromIntegral i + 2]
    


mkTablePop :: InsCmp CompileState m => Value -> m [Value]
mkTablePop tab = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- tableLen (toPointer tab)
    lenVal <- toVal len
    newLen <- mkIntInfix AST.Minus lenVal =<< toVal =<< newI64 1
    store (loc len) 0 (valOp newLen)

    mapM (\(Pointer t p) -> valLoad (Ptr t p)) =<< tableColumn (toPointer tab) newLen



mkTablePush :: InsCmp CompileState m => Value -> [Value] -> m Value
mkTablePush tab [] = do 
    len <- toVal =<< tableLen (toPointer tab)
    tableResize tab =<< mkIntInfix AST.Plus len =<< toVal =<< newI64 1
    ptrs <- tableColumn (toPointer tab) len
    forM_ ptrs $ \(Pointer pt po) -> valStore (Ptr pt po) =<< mkZero pt
    return len


tableDelete :: InsCmp CompileState m => Value -> Value -> m ()
tableDelete tab idx = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- toVal =<< tableLen (toPointer tab)
    end <- mkIntInfix AST.Minus len =<< toVal =<< newI64 1
    idxIsEnd <- mkIntInfix AST.EqEq idx end
    if_ (valOp idxIsEnd) (return ()) (idxNotEndCase end)
    void $ mkTablePop tab
    where
        idxNotEndCase :: InsCmp CompileState m => Value -> m ()
        idxNotEndCase end = do
            dsts <- tableColumn (toPointer tab) idx
            srcs <- tableColumn (toPointer tab) end
            zipWithM_ storeBasic dsts srcs
    

tableSetRow :: InsCmp CompileState m => Pointer -> Int -> Pointer -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- assertBaseType isTable (typeof tab)
    assert (typeof row == ts !! i) "Types do not match"
    pp <- gep (loc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (loc row)


tableResize :: InsCmp CompileState m => Value -> Value -> m ()
tableResize tab newLen = trace "tableResize" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType newLen)
    cap <- tableCap (toPointer tab)
    bFull <- mkIntInfix AST.GT newLen (fromPointer cap)
    if_ (valOp bFull) (fullCase ts) (return ())

    len <- tableLen (toPointer tab)
    store (loc len) 0 (valOp newLen)
    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            Val I64 newLenOp <- valLoad newLen
            newTab <- mkTable (valType tab) =<< Val I64 <$> mul newLenOp (int64 2)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                newRow <- tableRow i newTab
                row <- tableRow i tab
                memCpy newRow row =<< tableLen (toPointer tab)
            valStore tab newTab
