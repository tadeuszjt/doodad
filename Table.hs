{-# LANGUAGE FlexibleContexts #-}
module Table where

import Data.Word
import Control.Monad

import           LLVM.AST.Type              hiding (Type, void, double)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import qualified LLVM.AST.IntegerPredicate as P

import Monad
import Value
import CompileState
import Funcs
import qualified Type as T
import qualified AST as S

valTableLen :: InsCmp s m => Value -> m Value
valTableLen (Ptr (T.Table _) loc) = fmap (Ptr T.I64) $ gep loc [int32 0, int32 0]
valTableLen (Val (T.Table _) op)  = fmap (Val T.I64) $ extractValue op [0]


valTableCap :: InsCmp s m => Value -> m Value
valTableCap (Ptr (T.Table _) loc) = fmap (Ptr T.I64) $ gep loc [int32 0, int32 1]
valTableCap (Val (T.Table _) op)  = fmap (Val T.I64) $ extractValue op [1]


valTableRow :: InsCmp s m => Word32 -> Value -> m Value
valTableRow i val = do
    let T.Table ts = valType val
    assert (fromIntegral i < length ts) "table row index >= num rows"
    let t = ts !! fromIntegral i
    case val of
        Val _ op  -> fmap (Ptr t) (extractValue op [i+2])
        Ptr _ loc -> do
            pp <- gep loc [int32 0, int32 $ fromIntegral i+2]
            fmap (Ptr t) (load pp 0)


valTableSetRow :: InsCmp CompileState m => Word32 -> Value -> Value -> m ()
valTableSetRow i (Ptr (T.Table ts) loc) (Ptr t row) = do
    checkTypesMatch t (ts !! fromIntegral i)
    pp <- gep loc [int32 0, int32 $ fromIntegral i+2]
    store pp 0 row


valMalloc :: InsCmp CompileState m => T.Type -> Value -> m Value
valMalloc typ len = do
    Val T.I64 l <- valLoad len
    size <- sizeOf typ
    pi8 <- malloc =<< mul l (int64 $ fromIntegral size)
    opTyp <- opTypeOf typ
    fmap (Ptr typ) $ bitcast pi8 (ptr opTyp)


valTableForceAlloc :: InsCmp CompileState m => Value -> m Value
valTableForceAlloc tab@(Ptr typ _) = valTableForceAlloc' tab
valTableForceAlloc tab@(Val typ _) = do
    tab' <- valLocal typ
    valStore tab' tab
    valTableForceAlloc' tab'

valTableForceAlloc' tab@(Ptr _ _) = do
    T.Table ts <- baseTypeOf (valType tab)
    len <- valTableLen tab
    cap <- valTableCap tab
    
    let caseCapZero = do
        valStore cap len
        forM_ (zip ts [0..]) $ \(t, i) -> do
            mem <- valMalloc t len
            row <- valTableRow i tab
            valMemCpy mem row len
            valTableSetRow i tab mem

    z <- valsCompare S.EqEq cap (valInt T.I64 0)
    if_ (valOp z) caseCapZero (return ())
    return tab

--
--valTableAppend :: InsCmp CompileState m => Value -> Value -> m Value
--valTableAppend tab tup = do
--    T.Table tabTs <- baseTypeOf (valType tab)
--    T.Tuple tupTs <- baseTypeOf (valType tup)
--
--    -- create local table
--    loc <- valLocal (valType tab)
--    forM_ (zip tabTs [0..]) $ \(t, i) ->
--        valTableSetRow i loc =<< valTableRow i tab
--
--    cap <- valTableCap tab
--    len <- valTableLen tab
--    full <- valsCompare S.LTEq cap len
--    zero <- valsCompare S.LTEq cap (valInt T.I64 0)
--
--    -- change cap if zero
--    let zeroCase = do
--        l <- add (int64 2) =<< fmap valOp (valLoad len)
--        locCap <- valTableCap loc
--        valStore locCap (Val T.I64 l)
--
--    if_ (valOp zero) zeroCase (return ())
--
--
--    let fullCase = do
--        forM_ (zip tabTs [0..]) $ \(t, i) -> do
--            cap <- valTableCap loc
--            row <- valTableRow i loc
--            mem <- valMalloc t cap



--
--    forM_ (zip tabTs tupTs [0..]) $ \(tabT, tupT, i) -> do
--        checkTypesMatch tabT tupT
--        row <- valTableRow i tab
--        valTableSetRow loc i row
--        ptr <- valPtrIdx row len
--        valStore ptr =<< valTupleIdx tup i
--
--      return loc



