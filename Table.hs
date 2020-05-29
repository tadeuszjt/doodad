module Table where

import Control.Monad
import Data.List
import Data.Word
import Prelude                    hiding (EQ)

import LLVM.AST.Type              hiding (Type, void)
import LLVM.AST.IntegerPredicate
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Type
import CmpFuncs
import Value
import CmpMonad


cmpTableExpr :: [[Value]] -> Instr Value
cmpTableExpr rows = do
    let numRows = length rows
    assert (numRows > 0) "cannot deduce table type"
    let numCols = length (head rows)
    assert (all (== numCols) (map length rows)) "row lengths differ" 
    assert (numCols > 0) "cannot deduce table type"

    (rowTyps, rowTypSizes) <- fmap unzip $ forM rows $ \row -> do
        let typ = valType (head row)
        mapM_ (checkConcTypesMatch typ) (map valType row)
        size <- sizeOf typ
        return (typ, size)

    opTyp <- opTypeOf (Table Nothing rowTyps)
    typName <- freshName (mkBSS "table_t")
    addAction typName $ typedef typName (Just opTyp)
    ensureDef typName

    let typ = Table (Just typName) rowTyps
    let len = fromIntegral numCols

    tab <- valLocal typ
    tabLen <- valTableLen tab
    tabCap <- valTableCap tab
    valStore tabLen (valInt I64 len)
    valStore tabCap (valInt I64 0)

    forM_ (zip3 rows rowTyps [0..]) $ \(row, rowTyp, i) -> do
        arow <- valLocal $ Array (fromIntegral len) rowTyp
        valTableSetRow tab i =<< valCast rowTyp arow
        prow <- valTableRow i tab

        forM_ (zip row [0..]) $ \(val, j) -> do
            p <- valPtrIdx prow (valInt I64 j)
            valStore p val
            
    return tab


valTableCap :: Value -> Instr Value
valTableCap (Ptr (Table _ _) loc) = fmap (Ptr I64) $ gep loc [int32 0, int32 1]
valTableCap (Val (Table _ _) op)  = fmap (Val I64) $ extractValue op [1]


valTableRow :: Word32 -> Value -> Instr Value
valTableRow i tab = do
    Table nm ts <- realTypeOf (valType tab)
    assert (fromIntegral i < length ts) "invalid table row index" 
    maybe (return ()) ensureDef nm
    let t = ts !! fromIntegral i
    case tab of
        Val _ op  -> fmap (Ptr t) (extractValue op [i+2])
        Ptr _ loc -> do
            pp <- gep loc [int32 0, int32 $ fromIntegral i+2]
            fmap (Ptr t) (load pp 0)


valTableSetRow :: Value -> Word32 -> Value -> Instr ()
valTableSetRow (Ptr (Table _ ts) loc) idx (Ptr t row) = do
    checkTypesMatch t (ts !! fromIntegral idx)
    pp <- gep loc [int32 0, int32 (fromIntegral idx+2)]
    store pp 0 row



valTableIdx :: Value -> Value -> Instr Value
valTableIdx tab idx = do
    Table nm ts <- realTypeOf (valType tab)
    maybe (return ()) ensureDef nm
    tup <- valLocal (Tuple Nothing ts)

    forM_ (zip ts [0..]) $ \(t, i) -> do
        prow <- valTableRow i tab
        valTupleSet tup i =<< valPtrIdx prow idx

    if length ts == 1
    then valTupleIdx 0 tup
    else return tup


valMalloc :: Type -> Value -> Instr Value
valMalloc typ (Val I64 i) = do
    size  <- fmap fromIntegral (sizeOf typ)
    nBytes <- mul (int64 size) i
    sI64 <- fmap fromIntegral (sizeOf I64)
    nBytes' <- add nBytes (int64 sI64)
    pMem <- malloc nBytes'
    pI64 <- bitcast pMem (ptr i64)
    store pI64 0 (int64 0) -- ref count
    pI64' <- gep pI64 [int64 1]
    opTyp <- opTypeOf typ
    pVals <- bitcast pI64' (ptr opTyp)
    return (Ptr typ pVals)


valMallocIncRef :: Value -> Instr ()
valMallocIncRef (Ptr typ loc) = do
    pI64 <- bitcast loc (ptr i64)
    pRef <- gep pI64 [int64 $ -1]
    void $ store pRef 0 =<< add (int64 1) =<< load pRef 0


valMallocDecRef :: Value -> Instr ()
valMallocDecRef (Ptr typ loc) = do
    pI64 <- bitcast loc (ptr i64)
    pRef <- gep pI64 [int64 $ -1]
    pI8  <- bitcast pRef (ptr i8)
    ref  <- load pRef 0
    b    <- icmp EQ ref (int64 1)
    if_ b (void $ free pI8) $
        void $ store pRef 0 =<< add (int64 $ -1) =<< load pRef 0


valTableKill :: Value -> Instr ()
valTableKill tab = do
    let Table _ ts = valType tab
    let len = fromIntegral (length ts)
    forM_ [0..len-1] $ \i ->
        valMallocDecRef =<< valTableRow i tab
    len <- valTableLen tab
    cap <- valTableCap tab
    valStore len (valInt I64 0)
    valStore cap (valInt I64 0)



valTableStore :: Value -> Value -> Instr ()
valTableStore dest@(Ptr (Table nm ts) destLoc) src = do
    destConc <- concreteTypeOf (valType dest)
    srcConc <- concreteTypeOf (valType src)
    assert (destConc == srcConc) "table store"

    destLen <- valTableLen dest
    destCap <- valTableCap dest 
    len <- valLoad =<< valTableLen src
    cap <- valLoad =<< valTableCap src
    valStore destLen len
    valStore destCap len

    let false = valStore dest src
    let true = do
        forM_ (zip ts [0..]) $ \(t, i) -> do
            size <- sizeOf t

            m@(Ptr _ p) <- valMalloc t len
            valTableSetRow dest i m
            valMallocIncRef m

            srcRow <- valTableRow (fromIntegral i) src
            dstRow <- valTableRow (fromIntegral i) dest

            for (valOp len) $ \j -> do
                psrc <- valPtrIdx srcRow (Val I64 j)
                pdst <- valPtrIdx dstRow (Val I64 j)
                valTableStore pdst psrc

    Val Bool cap0 <- valsEqual cap (valInt I64 0)
    if_ cap0 true false
valTableStore dest src = valStore dest src


valTableAppend :: Value -> Value -> Instr Value
valTableAppend tab elem = do
    Table _ ts <- realTypeOf (valType tab)
    t <- realTypeOf (valType elem)
    case ts of
        [tt] -> do
            checkTypesMatch tt t

            cap <- valLoad =<< valTableCap tab
            len <- valLoad =<< valTableLen tab
            row  <- valTableRow 0 tab
            cap0 <- icmp EQ (valOp cap) (int64 0)

            val <- valLocal (valType tab)
            valLen <- valTableLen val
            valCap <- valTableCap val

            let cap0Case = do
                len2 <- fmap (Val I64) $ mul (valOp len) (int64 2)
                len' <- fmap (Val I64) $ add (valOp len) (int64 1)

                m    <- valMalloc t len2
                valMallocIncRef m
                valPtrMemCpy m row len

                valStore valLen len'
                valStore valCap len2
                valTableSetRow val 0 m

            if_ cap0 cap0Case trap

            valRow <- valTableRow 0 val
            p <- valPtrIdx valRow len
            valStore p elem
            return val




