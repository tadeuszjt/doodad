module CmpTable where

import           Control.Monad

import           LLVM.AST.Type
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import           CmpFuncs
import           CmpValue
import           CmpMonad


cmpTableExpr :: [[Value]] -> Instr Value
cmpTableExpr rows = do
    let numRows = length rows
    assert (numRows > 0) "cannot deduce table type"
    let numCols = length (head rows)
    assert (all (== numCols) (map length rows)) "row lengths differ" 
    assert (numCols > 0) "cannot deduce table type"

    (rowTyps, rowTypSizes) <- fmap unzip $ forM rows $ \row -> do
        let typ = valType (head row)
        assert (all (== typ) (map valType row)) "element types differ in row"
        size <- sizeOf =<< opTypeOf typ
        return (typ, size)

    let typ = Table rowTyps
    let len = numCols
    let cap = len

    opTyp <- opTypeOf typ
    typName <- freshName (mkBSS "table_t")
    addAction typName $ typedef typName (Just opTyp)
    ensureDef typName
    let namedTyp = typ

    name <- freshName (mkBSS "table")
    (val@(Ptr _ loc), ext) <- valGlobal name namedTyp
    lenPtr <- gep loc [int32 0, int32 0]
    store lenPtr 0 (int32 $ fromIntegral len)
    capPtr <- gep loc [int32 0, int32 1]
    store capPtr 0 (int32 $ fromIntegral cap)

    forM_ (zip3 rows rowTypSizes [0..]) $ \(row, size, i) -> do
        rowPtr <- gep loc [int32 0, int32 (i+2)]
        store rowPtr 0 =<< malloc (int64 $ fromIntegral cap * fromIntegral size)
        forM_ (zip row [0..]) $ \(val, j) -> do
            rowPtrVal <- load rowPtr 0
            pi8 <- gep rowPtrVal [int64 j]
            opTyp <- opTypeOf (valType val)
            elemPtr <- bitcast pi8 (ptr opTyp)
            valStore (Ptr (valType val) elemPtr) val
            

    return val
    
