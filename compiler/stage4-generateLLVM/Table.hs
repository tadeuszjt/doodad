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
    lenP <- mkAlloca I64
    valStore lenP initialLen

    tab <- newVal typ
    cap <- tableCap tab
    len <- tableLen tab
    storeBasic cap (Pointer I64 $ valLoc lenP)
    storeBasic len (Pointer I64 $ valLoc lenP)

    siz <- newI64 0
    idxs <- forM ts $ \t -> do
        idx <- valLoad (Ptr I64 $ loc siz)
        Val I64 op <- mkIntInfix AST.Plus (Ptr I64 $ loc siz) =<< mkIntInfix AST.Times initialLen =<< sizeOf t
        store (loc siz) 0 op
        return idx

    mal <- mkMalloc I8 (Ptr I64 $ loc siz)
    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        Ptr _ pi8 <- ptrIdx mal idx
        ptr <- fmap (Ptr t) $ bitcast pi8 =<< LL.ptr <$> opTypeOf t
        tableSetRow (Ptr typ $ loc tab) i ptr

    return (Ptr typ $ loc tab)


tableColumn :: InsCmp CompileState m => Value -> Value -> m [Pointer]
tableColumn tab idx = trace "tableGetElem" $ do
    Table ts <- baseTypeOf (valType tab)
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
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
    len <- toVal =<< tableLen (toPointer tab)
    newLen <- mkIntInfix AST.Minus len (mkI64 1)
    tableSetLen tab newLen
    mapM (\(Pointer t p) -> valLoad (Ptr t p)) =<< tableColumn tab newLen



mkTablePush :: InsCmp CompileState m => Value -> [Value] -> m Value
mkTablePush tab [] = do 
    len <- toVal =<< tableLen (toPointer tab)
    tableResize tab =<< mkIntInfix AST.Plus len (mkI64 1)
    ptrs <- tableColumn tab len
    forM_ ptrs $ \(Pointer pt po) -> valStore (Ptr pt po) =<< mkZero pt
    return len


tableDelete :: InsCmp CompileState m => Value -> Value -> m ()
tableDelete tab idx = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- toVal =<< tableLen (toPointer tab)
    end <- mkIntInfix AST.Minus len (mkI64 1)
    idxIsEnd <- mkIntInfix AST.EqEq idx end
    if_ (valOp idxIsEnd) (return ()) (idxNotEndCase end)
    void $ mkTablePop tab
    where
        idxNotEndCase :: InsCmp CompileState m => Value -> m ()
        idxNotEndCase end = do
            dsts <- tableColumn tab idx
            srcs <- tableColumn tab end
            zipWithM_ storeBasic dsts srcs
    

tableSetLen :: InsCmp CompileState m => Value -> Value -> m ()
tableSetLen tab@(Ptr _ loc) len = trace "tableSetLen" $ do
    Table _ <- assertBaseType isTable (valType tab)
    assertBaseType (==I64) (valType len)
    l <- gep loc [int32 0, int32 0]
    store l 0 . valOp =<< valLoad len


tableSetRow :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assert (valType row == ts !! i) "Types do not match"
    pp <- gep (valLoc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (valLoc row)


tableResize :: InsCmp CompileState m => Value -> Value -> m ()
tableResize tab newLen = trace "tableResize" $ do
    Table ts <- assertBaseType isTable (valType tab)
    assertBaseType isInt (valType newLen)
    Pointer I64 capP <- tableCap $ Pointer (valType tab) (valLoc tab)
    bFull <- mkIntInfix AST.GT newLen (Ptr I64 capP)
    if_ (valOp bFull) (fullCase ts) (return ())
    tableSetLen tab newLen
    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            newTab <- mkTable (valType tab) =<< mkIntInfix AST.Times newLen (mkI64 2)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                (Pointer tn pn) <- tableRow i newTab
                (Pointer to po) <- tableRow i tab
                valMemCpy (Ptr tn pn) (Ptr to po) =<< toVal =<< tableLen (toPointer tab)
            valStore tab newTab
