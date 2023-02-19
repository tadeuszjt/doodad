{-# LANGUAGE OverloadedStrings #-}
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
    Table _ <- baseTypeOf table
    Pointer I64 <$> gep (loc table) [int32 0, int32 0]


tableCap :: InsCmp CompileState m => Pointer -> m Pointer
tableCap table = do
    Table _ <- baseTypeOf table
    Pointer I64 <$> gep (loc table) [int32 0, int32 1]


-- allocate a table with the specified type and length
newTable :: InsCmp CompileState m => Type -> Value -> m Pointer
newTable typ initialLen = do
    Table ts <- baseTypeOf typ
    I64 <- baseTypeOf initialLen

    tab <- newVal typ
    cap <- tableCap tab
    len <- tableLen tab
    storeBasicVal cap initialLen
    storeBasicVal len initialLen

    size <- newI64 0
    idxs <- forM ts $ \t -> do
        idx <- pload size
        storeBasicVal size =<< intInfix AST.Plus idx =<< intInfix AST.Times initialLen =<< sizeOf t
        return idx

    mal <- pMalloc I8 =<< pload size
    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        Pointer _ pi8 <- advancePointer mal idx
        ptr <- fmap (Pointer t) $ bitcast pi8 =<< LL.ptr <$> opTypeOf t
        tableSetRow tab i ptr

    return tab


tableColumn :: InsCmp CompileState m => Pointer -> Value -> m [Pointer]
tableColumn tab idx = trace "tableGetElem" $ do
    Table ts <- baseTypeOf tab
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- tableRow i tab
        advancePointer row idx


tableRow :: InsCmp CompileState m => Int -> Pointer -> m Pointer
tableRow i tab = do
    Table ts <- baseTypeOf tab
    assert (i >= 0 && i < length ts) "Invalid table row index"
    Pointer (ts !! i) <$> do 
        (flip load) 0 =<< gep (loc tab) [int32 0, int32 (fromIntegral i + 2)]
    


tablePush :: InsCmp CompileState m => Pointer -> m Value
tablePush tab = do 
    len <- pload =<< tableLen tab
    tableResize tab =<< intInfix AST.Plus len (mkI64 1)
    ptrs <- tableColumn tab len
    forM_ ptrs $ \ptr -> storeBasicVal ptr =<< mkZero (typeof ptr)
    return len


tableDelete :: InsCmp CompileState m => Pointer -> Value -> m ()
tableDelete tab idx = do
    Table ts <- baseTypeOf tab
    len <- tableLen tab
    lenv <- pload len
    end <- intInfix AST.Minus lenv (mkI64 1)

    -- swap len idx
    dsts <- tableColumn tab idx
    srcs <- tableColumn tab end
    zipWithM_ storeBasic dsts srcs

    -- reduce size
    storeBasicVal len end


tableSetRow :: InsCmp CompileState m => Pointer -> Int -> Pointer -> m ()
tableSetRow tab i row = trace "tableSetRow" $ do
    Table ts <- baseTypeOf tab
    assert (typeof row == ts !! i) "Types do not match"
    pp <- gep (loc tab) [int32 0, int32 (fromIntegral i+2)]
    store pp 0 (loc row)


tableResize :: InsCmp CompileState m => Pointer -> Value -> m ()
tableResize tab newLen = do
    exit <- freshName "tableResize_exit"
    needsResize <- freshName "tableResize_realloc_mem"

    Table ts <- baseTypeOf tab
    I64      <- baseTypeOf newLen

    cap <- tableCap tab
    len <- tableLen tab

    full <- intInfix AST.GT newLen =<< pload cap
    condBr (op full) needsResize exit

    emitBlockStart needsResize -- TODO this needs to clear elems
    newTab <- newTable (typeof tab) =<< intInfix AST.Times newLen (mkI64 2)
    forM_ (zip ts [0..]) $ \(t, i) -> do
        newRow <- tableRow i newTab
        row <- tableRow i tab
        memCpy newRow row =<< pload len
    storeBasic tab newTab
    br exit

    emitBlockStart exit
    storeBasicVal len newLen
