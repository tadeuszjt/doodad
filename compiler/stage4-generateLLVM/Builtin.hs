{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Builtin where

import Prelude hiding (and, or)
import Control.Monad

import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as P
import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.AST.Name
import LLVM.AST.Global hiding (prefix)
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import qualified LLVM.AST.Constant as C

import qualified AST
import Type
import Value
import State
import Monad
import Error
import Typeof
import Trace
import Table
import Tuple
import ADT
import Funcs
import Symbol
import Array
import Sparse


-- Builtin contains functions that operate on Values which require the
-- specialised Value functions such as Table, Array, Tuple etc. 


-- guarantees an equivalent value in a different memory location
storeCopy :: InsCmp CompileState m => Pointer -> Pointer -> m ()
storeCopy dst src = withErrorPrefix "storeCopy: " $ do
    baseDst <- baseTypeOf dst
    baseSrc <- baseTypeOf src
    True <- return $ baseDst == baseSrc
    case baseDst of
        _ | isSimple baseDst -> storeBasic dst src
        Table ts -> do 
            len <- pload =<< tableLen src
            tableResize dst len
            for (op len) $ \n -> do 
                columnDst <- tableColumn dst (Value2 I64 n)
                columnSrc <- tableColumn src (Value2 I64 n)
                zipWithM_ storeCopy columnDst columnSrc

        _ -> fail (show baseDst)


tablePop :: InsCmp CompileState m => Pointer -> m [Pointer]
tablePop tab = do
    Table ts <- baseTypeOf tab
    len <- tableLen tab
    lenv <- pload len
    newLen <- intInfix AST.Minus lenv =<< pload =<< newI64 1
    storeBasicVal len newLen
    ptrs <- tableColumn tab newLen
    forM ptrs $ \ptr -> do 
        val <- newVal (typeof ptr)
        storeCopy val ptr
        return val


storeCopyVal :: InsCmp CompileState m => Pointer -> Value2 -> m ()
storeCopyVal dst src = withErrorPrefix "storeCopy: " $ do
    baseDst <- baseTypeOf dst
    baseSrc <- baseTypeOf src
    True <- return $ baseDst == baseSrc
    case baseDst of
        _ | isSimple baseDst -> storeBasicVal dst src
        _ -> fail (show baseDst)

sparsePush :: InsCmp CompileState m => Pointer -> [Pointer] -> m Value2
sparsePush sparse elems = do
    Sparse ts <- baseTypeOf sparse
    assert (map typeof elems == ts) "Elem types do not match"
    stack <- sparseStack sparse
    stackLen <- pload =<< tableLen stack
    stackLenGTZero <- intInfix AST.GT stackLen =<< pload =<< newI64 0
    ret <- newI64 0 
    if_ (op stackLenGTZero) (popStackCase stack ret) (pushTableCase ret) 
    pload ret
    where
        popStackCase :: InsCmp CompileState m => Pointer -> Pointer -> m ()
        popStackCase stack ret = do
            [idx] <- tablePop stack
            table <- sparseTable sparse
            column <- tableColumn table =<< pload idx
            zipWithM_ storeCopy column elems 
            storeBasic ret idx
            
        pushTableCase :: InsCmp CompileState m => Pointer -> m ()
        pushTableCase ret = do
            table <- sparseTable sparse
            len <- pload =<< tableLen table
            tableResize table =<< intInfix AST.Plus len =<< pload =<< newI64 1
            column <- tableColumn table len 
            zipWithM_ storeCopy column elems 
            valStore (fromPointer ret) (fromValue len)

-- construct a value from arguments, Eg. i64(3.2), Vec2(12, 43)
construct :: InsCmp CompileState m => Type -> [Pointer] -> m Pointer
construct typ args = do 
    val <- newVal typ
    base <- baseTypeOf typ
    case args of 
        [] -> return () 
        [v] | isIntegral base -> storeBasicVal val =<< convertNumber typ =<< pload v
        [v] | isFloat base    -> storeBasicVal val =<< convertNumber typ =<< pload v
    return val


-- convert the value into a new value corresponding to the type
newConvert :: InsCmp CompileState m => Type -> Pointer -> m Pointer
newConvert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf val
    ptr <- newVal typ
    case base of
        _ | isIntegral base -> storeBasicVal ptr =<< convertNumber typ =<< pload val
        _ | isFloat base    -> storeBasicVal ptr =<< convertNumber typ =<< pload val
        _ | baseVal == base -> storeCopy ptr val
        _ -> fail ("valConvert " ++ show base)
    return ptr


min :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
min a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    base <- baseTypeOf a
    cnd <- mkInfix AST.GT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- pload a 
            valB <- pload b
            store (loc val) 0 =<< select (valOp cnd) (op valB) (op valA)
    return val


max :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
max a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    base <- baseTypeOf a
    cnd <- mkInfix AST.LT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- pload a 
            valB <- pload b
            store (loc val) 0 =<< select (valOp cnd) (op valB) (op valA)
    return val


-- any infix expression
mkInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Value
mkInfix operator a b = withErrorPrefix "infix: " $ do
    assert (typeof a == typeof b) $ "type mismatch: " ++ show (typeof a, typeof b)
    base <- baseTypeOf a
    case base of
        Bool                 -> do 
            av <- pload a
            fromValue <$> (boolInfix operator av =<< pload b)
        Char                 -> do 
            av <- pload a
            fromValue <$> (intInfix operator av =<< pload b)
        Enum                 -> do 
            av <- pload a
            bv <- pload b
            fromValue <$> enumInfix operator av bv
        _ | isRange base     -> toVal =<< rangeInfix operator a b
        _ | isInt base       -> do 
            av <- pload a
            fromValue <$> (intInfix operator av =<< pload b)
        _ | isFloat base     -> do 
            av <- pload a
            fromValue <$> (floatInfix operator av =<< pload b)
        _ | isTuple base     -> mkTupleInfix operator (fromPointer a) (fromPointer b)
        _ | isArray base     -> mkArrayInfix operator (fromPointer a) (fromPointer b)
        _ | isTable base     -> mkTableInfix operator (fromPointer a) (fromPointer b)
        _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show (typeof a) ++ " " ++ show (typeof b)



enumInfix :: InsCmp CompileState m => AST.Operator -> Value2 -> Value2 -> m Value2
enumInfix operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    Enum <- baseTypeOf a
    case operator of
        AST.NotEq -> Value2 Bool <$> icmp P.NE (op a) (op b)
        AST.EqEq  -> Value2 Bool <$> icmp P.EQ (op a) (op b)


rangeInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
rangeInfix operator a b = do
    Range t <- baseTypeOf a
    True    <- return $ typeof a == typeof b
    case operator of
        AST.EqEq -> do
            val <- newVal Bool
            startA <- rangeStart a
            startB <- rangeStart b
            endA   <- rangeEnd a
            endB   <- rangeEnd b
            startEq <- mkInfix AST.EqEq startA startB
            endEq   <- mkInfix AST.EqEq endA endB
            storeBasicVal val =<< Value2 Bool <$> and (valOp startEq) (valOp endEq)
            return val



mkArrayInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkArrayInfix operator a b = withErrorPrefix "array" $ do
    assert (typeof a == typeof b) "infix type mismatch"
    Array n t <- assertBaseType isArray (typeof a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            arr <- newVal (typeof a)
            forM_ [0..n] $ \i -> do
                pDst <- ptrArrayGetElemConst (fromPointer arr) i
                pSrcA <- ptrArrayGetElemConst a i
                pSrcB <- ptrArrayGetElemConst b i
                valStore pDst =<< mkInfix operator (toPointer pSrcA) (toPointer pSrcB)
            return (fromPointer arr)


mkTupleInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkTupleInfix operator a b = withErrorPrefix "tuple infix: " $ do
    assert (typeof a == typeof b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (typeof a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            tup <- newVal (typeof a)
            forM (zip ts [0..]) $ \(t, i) -> do
                pSrcA <- tupleIdx i (toPointer a)
                pSrcB <- tupleIdx i (toPointer b)
                pDst  <- tupleIdx i tup
                valStore (fromPointer pDst) =<< mkInfix operator pSrcA pSrcB
            return (fromPointer tup)

        _ | operator `elem` [AST.EqEq] -> do
            res <- newBool True
            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i (toPointer a)
                valB <- tupleIdx i (toPointer b)
                equal <- mkInfix AST.EqEq valA valB
                cond <- freshName "tuple_eqeq_fail"
                condBr (valOp equal) (cases !! (i + 1)) cond
                emitBlockStart cond
                storeBasic res =<< newBool False
                br exit

            emitBlockStart exit
            toVal res

        _ | operator `elem` [AST.GTEq] -> do
            deflt <- case operator of
                AST.GTEq -> return True
            res <- newBool deflt

            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i (toPointer a)
                valB <- tupleIdx i (toPointer b)
                equal <- mkInfix AST.EqEq valA valB
                cond <- freshName "tuple_gt_cond"
                condBr (valOp equal) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                valStore (fromPointer res) =<< mkInfix operator valA valB
                br exit

            emitBlockStart exit
            toVal res

        _ -> error (show operator)
                    
        
mkTableInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkTableInfix operator a b = do
    assertBaseType isTable (typeof a)
    assert (typeof a == typeof b) "type mismatch"
    let typ = typeof a

    lenA <- pload =<< tableLen (toPointer a)
    lenB <- tableLen (toPointer b)
    lenEq <- intInfix AST.EqEq lenA =<< pload lenB

    case operator of
        AST.NotEq -> fromValue <$> (prefix AST.Not . toValue =<< mkTableInfix AST.EqEq a b)
        AST.EqEq  -> do
            eq <- newBool False

            exit <- freshName "eqeq_table_exit"
            start <- freshName "eqeq_table_start"
            cond <- freshName "eqeq_table_cond"
            body <- freshName "eqeq_table_body"

            -- test that len(a) == len(b)
            condBr (op lenEq) start exit
            emitBlockStart start
            idx <- newI64 0
            storeBasic eq =<< newBool True
            br cond

            -- test that the idx < len
            emitBlockStart cond
            idxv0 <- pload idx
            idxLT <- intInfix AST.LT idxv0 lenA
            condBr (op idxLT) body exit

            -- test that a[i] == b[i]
            emitBlockStart body
            [columnA] <- tableColumn (toPointer a) =<< pload idx
            [columnB] <- tableColumn (toPointer b) =<< pload idx
            elmEq <- mkInfix AST.EqEq columnA columnB
            valStore (fromPointer eq) elmEq
            idxv <- pload idx
            valStore (fromPointer idx) . fromValue =<< intInfix AST.Plus idxv =<< pload =<< newI64 1
            condBr (valOp elmEq) cond exit

            emitBlockStart exit
            toVal eq


valAdtNormalInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
valAdtNormalInfix operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    ADT fs <- assertBaseType isADT (typeof a)
    case operator of
        AST.NotEq -> fromValue <$> (prefix AST.Not . toValue =<< valAdtNormalInfix AST.EqEq a b)
        AST.EqEq -> do
            enA <- adtEnum a
            enB <- adtEnum b
            enEq <- intInfix AST.EqEq enA enB

            -- if enum isn't matched, exit
            match <- newBool False
            start <- freshName "start"
            exit <- freshName "exit"
            condBr (op enEq) start exit

            -- enum matched, match args
            emitBlockStart start
            storeBasic match =<< newBool True

            -- select block based on enum
            caseNames <- replicateM (length fs) (freshName "case")
            switch (op enA) exit $ zip (map (toCons . int64) [0..]) caseNames

            forM_ (zip caseNames [0..]) $ \(caseName, i) -> do
                emitBlockStart caseName

                bs <- case fs !! i of
                    FieldNull -> fmap (\a -> [a]) $ pload =<< newBool True
                    FieldType t -> do
                        valA <- adtDeref a i 0
                        valB <- adtDeref b i 0
                        valBp <- newVal (typeof valB)
                        valAp <- newVal (typeof valA)
                        valStore (fromPointer valBp) valB
                        valStore (fromPointer valAp) valA
                        fmap (\a -> [a]) $ toValue <$> mkInfix AST.EqEq valAp valBp
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref a i j
                            valB <- adtDeref b i j
                            valBp <- newVal (typeof valB)
                            valAp <- newVal (typeof valA)
                            valStore (fromPointer valBp) valB
                            valStore (fromPointer valAp) valA
                            toValue <$> mkInfix AST.EqEq valAp valBp

                true <- pload =<< newBool True
                storeBasicVal match =<< foldM (boolInfix AST.EqEq) true bs
                br exit

            emitBlockStart exit
            toVal match

