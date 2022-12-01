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
import qualified IR
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

tableTypeDef :: InsCmp CompileState m => Symbol -> AST.AnnoType -> m ()
tableTypeDef symbol (AST.AnnoType typ) = trace "tableTypeDef" $ do
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
        valStore siz =<< mkIntInfix AST.Plus siz =<< mkIntInfix AST.Times len =<< sizeOf t
        return idx

    mal <- mkMalloc I8 siz
    forM_ (zip3 ts idxs [0..]) $ \(t, idx, i) -> do
        Ptr _ pi8 <- ptrIdx mal idx
        ptr <- fmap (Ptr t) $ bitcast pi8 =<< LL.ptr <$> opTypeOf t
        tableSetRow tab i ptr

    return tab


ptrsTableColumn :: InsCmp CompileState m => Value -> Value -> m [Value]
ptrsTableColumn tab idx = trace "tableGetElem" $ do
    Table ts <- assertBaseType isTable (valType tab)
    forM (zip ts [0..]) $ \(t, i) -> do
        row <- ptrTableRow i tab
        ptrIdx row idx


ptrTableRow :: InsCmp CompileState m => Int -> Value -> m Value
ptrTableRow i tab = do
    Table ts <- assertBaseType isTable (valType tab)
    assert (i >= 0 && i < length ts) "Invalid table row index"
    Ptr (ts !! i) <$> case tab of
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 (fromIntegral i + 2)]
        Val _ op  -> extractValue op [fromIntegral i + 2]
    


valTablePop :: InsCmp CompileState m => Value -> m [Value]
valTablePop tab = do
    Table ts <- assertBaseType isTable (valType tab)
    len <- mkTableLen tab
    newLen <- mkIntInfix AST.Minus len (mkI64 1)
    tableSetLen tab newLen
    mapM valLoad =<< ptrsTableColumn tab newLen


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
    bFull <- mkIntInfix AST.GT newLen =<< mkTableCap tab
    if_ (valOp bFull) (fullCase ts) (return ())
    tableSetLen tab newLen
    where
        fullCase :: InsCmp CompileState m => [Type] -> m ()
        fullCase ts = do
            newTab <- mkTable (valType tab) =<< mkIntInfix AST.Times newLen (mkI64 2)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                newRow <- ptrTableRow i newTab
                oldRow <- ptrTableRow i tab
                valMemCpy newRow oldRow =<< mkTableLen tab
            valStore tab newTab
