module CmpTable where

import Control.Monad
import Data.List
import Data.Word

import LLVM.AST.Type
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Type
import CmpFuncs
import CmpValue
import CmpMonad


cmpTableExpr :: [[Value]] -> Instr Value
cmpTableExpr rows = do
    let numRows = length rows
    assert (numRows > 0) "cannot deduce table type"
    let numCols = length (head rows)
    assert (all (== numCols) (map length rows)) "row lengths differ" 
    assert (numCols > 0) "cannot deduce table type"

    (rowTyps, rowOpTyps, rowTypSizes) <- fmap unzip3 $ forM rows $ \row -> do
        let typ = valType (head row)
        assert (all (== typ) (map valType row)) "element types differ in row"
        opTyp <- opTypeOf typ
        size <- sizeOf opTyp
        return (typ, opTyp, size)

    typName <- freshName (mkBSS "table_t")
    opTyp <- opTypeOf (Table Nothing rowTyps)
    addAction typName $ typedef typName (Just opTyp)
    ensureDef typName

    let typ = Table (Just typName) rowTyps
    let len = fromIntegral numCols

    tab@(Ptr (Table _ _) loc) <- valLocal typ
    lenPtr <- gep loc [int32 0, int32 0]
    capPtr <- gep loc [int32 0, int32 1]
    store lenPtr 0 (int32 len)
    store capPtr 0 (int32 0)

    forM_ (zip4 rows rowTyps rowOpTyps [0..]) $ \(row, rowTyp, rowOpTyp, i) -> do
        Ptr _ p <- valLocal $ Array (fromIntegral len) rowTyp
        pMem <- bitcast p (ptr rowOpTyp)

        rowPtr <- gep loc [int32 0, int32 (i+2)]
        store rowPtr 0 pMem

        forM_ (zip row [0..]) $ \(val, j) -> do
            rowPtrVal <- load rowPtr 0
            pi8 <- gep rowPtrVal [int64 j]
            opTyp <- opTypeOf (valType val)
            elemPtr <- bitcast pi8 (ptr opTyp)
            valStore (Ptr (valType val) elemPtr) val
            
    return tab


valTableLen :: Value -> Instr Value
valTableLen (Ptr _ loc) = fmap (Ptr I64) $ gep loc [int32 0, int32 0]
valTableLen (Val _ op)  = fmap (Val I64) $ extractValue op [0]


valTableCap :: Value -> Instr Value
valTableCap (Ptr _ loc) = fmap (Ptr I64) $ gep loc [int32 0, int32 1]
valTableCap (Val _ op)  = fmap (Val I64) $ extractValue op [1]


valTableRow :: Word32 -> Value -> Instr Value
valTableRow i tab = do
    Table nm ts <- nakedTypeOf (valType tab)
    maybe (return ()) ensureDef nm
    case tab of
        Ptr _ loc -> do
            pp <- gep loc [int32 0, int32 (fromIntegral i + 2)]
            p <- load pp 0
            return $ Ptr (ts !! fromIntegral i) p
        Val _ op -> do
            p <- extractValue op [i+2]
            return $ Ptr (ts !! fromIntegral i) p


valTableIdx :: Value -> Value -> Instr Value
valTableIdx tab idx = do
    Table nm ts <- nakedTypeOf (valType tab)
    idxTyp      <- nakedTypeOf (valType idx)
    Val _ idxOp <- valLoad idx

    assert (isInt idxTyp) "index isn't int"
    maybe (return ()) ensureDef nm
    tup <- valLocal (Tuple Nothing ts)

    case tab of
        Ptr _ loc ->
            forM_ (zip ts [0..]) $ \(t, i) -> do
                p <- (flip load) 0 =<< gep loc [int32 0, int32 (i+2)]
                ep <- gep p [idxOp]
                valTupleSet tup (fromIntegral i) (Ptr t ep)

    if length ts == 1
    then valTupleIdx 0 tup
    else return tup


valTableStore :: Value -> Value -> Instr ()
valTableStore dest@(Ptr _ destLoc) src = do
    destConc <- concreteTypeOf (valType dest)
    srcConc <- concreteTypeOf (valType src)
    assert (destConc == srcConc) "table store"
    let Table nm ts = destConc

    pDestLen <- valTableLen dest
    pDestCap <- valTableCap dest 
    len <- valLoad =<< valTableLen src
    valStore pDestLen len
    valStore pDestCap len

    forM_ (zip ts [0..]) $ \(t, i) -> do
        opTyp <- opTypeOf t
        size <- sizeOf opTyp
        pp <- gep destLoc [int32 0, int32 (i+2)]
        nb <- mul (int64 $ fromIntegral size) (valOp len)
        ma <- malloc nb
        mb <- bitcast ma (ptr opTyp)
        store pp 0 mb
        Ptr _ ms <- valTableRow (fromIntegral i) src
        memcpy ma ms nb

