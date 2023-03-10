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
import Map


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
        Range t -> storeBasic dst src
        Key t -> storeBasic dst src

        Table ts -> do 
            len <- pload =<< tableLen src
            tableResize dst len

            forM_ (zip ts [0..]) $ \(t, i) -> do 
                srcRow <- tableRow i src
                dstRow <- tableRow i dst
                base <- baseTypeOf t
                isDataType <- isDataType t
                if isDataType then do
                    for (op len) $ \n -> do 
                        srcElem <- advancePointer srcRow (Value I64 n)
                        dstElem <- advancePointer srcRow (Value I64 n)
                        storeCopy dstElem srcElem
                else do 
                    memCpy dstRow srcRow len

        Tuple ts -> do 
            forM_ [0..length ts - 1] $ \i -> do 
                dstElem <- tupleIdx i dst
                srcElem <- tupleIdx i src
                storeCopy dstElem srcElem

        ADT fs -> storeBasic dst src

        Array n t -> do
            forM_ [0..n-1] $ \i -> do
                pDst <- arrayGetElem dst (mkI64 i)
                pSrc <- arrayGetElem src (mkI64 i)
                storeCopy pDst pSrc

        _ -> error (show baseDst)



storeCopyVal :: InsCmp CompileState m => Pointer -> Value -> m ()
storeCopyVal dst src = do
    baseDst <- baseTypeOf dst
    baseSrc <- baseTypeOf src
    True <- return $ baseDst == baseSrc
    case baseDst of
        _ | isSimple baseDst -> storeBasicVal dst src
        Key _ -> storeBasicVal dst src
        _ -> error (show baseDst)



tablePop :: InsCmp CompileState m => Pointer -> m [Pointer]
tablePop tab = do
    Table ts <- baseTypeOf tab
    len <- tableLen tab
    lenv <- pload len
    newLen <- intInfix AST.Minus lenv (mkI64 1)
    storeBasicVal len newLen
    ptrs <- tableColumn tab newLen
    forM ptrs $ \ptr -> do 
        val <- newVal (typeof ptr)
        storeCopy val ptr
        return val


mapIndex :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer 
mapIndex map key = do 
    Map tk tv <- baseTypeOf map
    foundCase <- freshName "mapIndex_found"
    notFoundCase <- freshName "mapIndex_not_found"
    exit <- freshName "mapIndex_exit"

    opType <- opTypeOf tv
    ptr <- alloca (LL.ptr opType) Nothing 0
    (found, foundIdx) <- mapFind map key
    condBr (op found) foundCase notFoundCase

    emitBlockStart foundCase
    row <- mapValues map
    elm <- advancePointer row foundIdx
    store ptr 0 (loc elm)
    br exit

    emitBlockStart notFoundCase
    vp <- mapPush map key
    store ptr 0 (loc vp)
    br exit

    emitBlockStart exit
    Pointer tv <$> load ptr 0


mapPush :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
mapPush map key = do
    Map tk tv <- baseTypeOf map
    idx <- tablePush (Pointer (Table [tk, tv]) (loc map))
    [pk, pv] <- tableColumn (Pointer (Table [tk, tv]) (loc map)) idx
    storeCopy pk key
    return pv


mapFind :: InsCmp CompileState m => Pointer -> Pointer -> m (Value, Value) --found, idx
mapFind map key = do
    Map tk tv <- baseTypeOf map
    exit <- freshName "mapFind_exit"
    cond <- freshName "mapFind_cond"
    eqeq <- freshName "mapFind_EqEq"
    incr <- freshName "mapFind_incr"

    len <- pload =<< mapLen map
    idx <- newI64 0
    keys <- mapKeys map -- keyPtr
    found <- newVal Bool
    br cond

    emitBlockStart cond
    idxLT <- intInfix AST.GT len =<< pload idx
    condBr (op idxLT) eqeq exit

    emitBlockStart eqeq
    elm <- advancePointer keys =<< pload idx
    storeInfix found AST.EqEq elm key
    eq <- pload found
    condBr (op eq) exit incr

    emitBlockStart incr
    increment idx
    br cond

    emitBlockStart exit
    f <- pload found
    i <- pload idx 
    return (f, i)


sparsePush :: InsCmp CompileState m => Pointer -> [Pointer] -> m Value
sparsePush sparse elems = do
    Sparse ts <- baseTypeOf sparse
    assert (map typeof elems == ts) "Elem types do not match"
    stack <- sparseStack sparse
    stackLen <- pload =<< tableLen stack
    stackLenGTZero <- intInfix AST.GT stackLen (mkI64 0)
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
            tableResize table =<< intInfix AST.Plus len (mkI64 1)
            column <- tableColumn table len 
            zipWithM_ storeCopy column elems 
            storeCopyVal ret len


-- construct a value from arguments, Eg. i64(3.2), Vec2(12, 43)
storeConstruct :: InsCmp CompileState m => Pointer -> [Pointer] -> m ()
storeConstruct location args = do 
    base <- baseTypeOf location
    case (args, base) of 
        ([], _) -> return () 
        ([v], base) | isIntegral base -> storeBasicVal location =<< convertNumber (typeof location) =<< pload v
        ([v], base) | isFloat base    -> storeBasicVal location =<< convertNumber (typeof location) =<< pload v
        ([v], ADT fs) -> do
            i <- adtTypeField (typeof location) (typeof v)
            adtSetEnum location i
            p <- adtField location i
            storeCopy p v
        
        (vs, Tuple ts) -> do
            assert (map typeof vs == ts) "invalid constructor"
            forM_ (zip vs [0..]) $ \(v, i) -> do
                ptr <- tupleIdx i location
                storeCopy ptr v

        _ -> error $ show (args, base)


prefix :: InsCmp CompileState m => AST.Operator -> Pointer -> m Value
prefix operator val = do
    base <- baseTypeOf val
    Value (typeof val) <$> case base of
        _ | isInt base -> case operator of
            AST.Plus -> op <$> pload val
            AST.Minus -> do
                zero <- newVal (typeof val)
                storeInfix zero AST.Minus zero val
                op <$> pload zero

        _ | isFloat base -> case operator of
            AST.Plus -> op <$> pload val
            AST.Minus -> do 
                mkFloat (typeof val) 0 >>= \zero -> fsub (op zero) . op =<< pload val

        Bool -> case operator of
            AST.Not -> do 
                o <- op <$> pload val
                icmp P.EQ o (bit 0)

        Char -> case operator of
            AST.Minus -> sub (int8 0) . op =<< pload val

        _ -> fail $ show base
        

min :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
min a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    cnd <- newVal Bool
    storeInfix cnd AST.GT a b
    val <- newVal (typeof a)
    right <- freshName "right"
    left <- freshName "left"
    exit <- freshName "exit"

    c <- pload cnd
    condBr (op c) right left
    emitBlockStart right
    storeCopy val b
    br exit

    emitBlockStart left
    storeCopy val a
    br exit

    emitBlockStart exit
    return val


max :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
max a b = withErrorPrefix "max: " $ do
    assert (typeof a == typeof b) $ show (typeof a) ++ " != " ++ show (typeof b)
    cnd <- newVal Bool
    storeInfix cnd AST.GT a b
    val <- newVal (typeof a)
    right <- freshName "right"
    left <- freshName "left"
    exit <- freshName "exit"

    c <- pload cnd
    condBr (op c) left right
    emitBlockStart right
    storeCopy val b
    br exit

    emitBlockStart left
    storeCopy val a
    br exit
    
    emitBlockStart exit
    return val


storeInfix :: InsCmp CompileState m => Pointer -> AST.Operator -> Pointer -> Pointer -> m ()
storeInfix location operator a b = withErrorPrefix "infix: " $ do
    assert (typeof a == typeof b) $ "type mismatch: " ++ show (typeof a, typeof b)
    base <- baseTypeOf a
    case base of
        ADT _ -> storeAdtInfix location operator a b
        Tuple _ -> storeTupleInfix location operator a b
        Array _ _ -> storeArrayInfix location operator a b
        Range _ -> storeCopyVal location =<< rangeInfix operator a b
        Table _ -> storeTableInfix location operator a b
        Bool                 -> do 
            av <- pload a
            storeCopyVal location =<< boolInfix operator av =<< pload b
        Char                 -> do 
            av <- pload a
            storeCopyVal location =<< intInfix operator av =<< pload b
        Key _                 -> do 
            av <- pload a
            storeCopyVal location =<< keyInfix operator av =<< pload b
        Enum -> do 
            av <- pload a
            bv <- pload b
            storeCopyVal location =<< enumInfix operator av bv
        _ | isInt base       -> do 
            av <- pload a
            storeCopyVal location =<< intInfix operator av =<< pload b
        _ | isFloat base     -> do 
            av <- pload a
            storeCopyVal location =<< floatInfix operator av =<< pload b
        _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show (typeof a) ++ " " ++ show (typeof b)


enumInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
enumInfix operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    Enum <- baseTypeOf a
    case operator of
        AST.NotEq -> Value Bool <$> icmp P.NE (op a) (op b)
        AST.EqEq  -> Value Bool <$> icmp P.EQ (op a) (op b)


rangeInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Value
rangeInfix operator a b = do
    Range t <- baseTypeOf a
    True    <- return $ typeof a == typeof b
    case operator of
        AST.EqEq -> do
            startA <- rangeStart a
            endA   <- rangeEnd a
            eq <- newVal Bool
            storeInfix eq AST.EqEq startA =<< rangeStart b
            oldEq <- pload eq
            storeInfix eq AST.EqEq endA =<< rangeEnd b
            boolInfix AST.AndAnd oldEq =<< pload eq


storeArrayInfix :: InsCmp CompileState m => Pointer -> AST.Operator -> Pointer -> Pointer -> m ()
storeArrayInfix location operator a b = do
    assert (typeof a == typeof b) "infix type mismatch"
    Array n t <- baseTypeOf a
    isDataType <- isDataType t

    case operator of
        AST.EqEq | not isDataType -> do 
            pA <- arrayGetElem a (mkI64 0)
            pB <- arrayGetElem b (mkI64 0)
            storeCopyVal location =<< intInfix AST.EqEq (mkI64 0) =<< memCmp pA pB (mkI64 n)

        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            forM_ [0..n] $ \i -> do
                pDst <- arrayGetElem location (mkI64 i)
                pSrcA <- arrayGetElem a (mkI64 i)
                pSrcB <- arrayGetElem b (mkI64 i)
                storeInfix pDst operator pSrcA pSrcB

storeTupleInfix :: InsCmp CompileState m => Pointer -> AST.Operator -> Pointer -> Pointer -> m ()
storeTupleInfix location operator a b = withErrorPrefix "tuple infix: " $ do
    assert (typeof a == typeof b) "type mismatch"
    Tuple ts <- baseTypeOf a

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            forM_ (zip ts [0..]) $ \(t, i) -> do
                pSrcA <- tupleIdx i a
                pSrcB <- tupleIdx i b
                pDst  <- tupleIdx i location
                storeInfix pDst operator pSrcA pSrcB

        _ | operator `elem` [AST.EqEq] -> do
            storeCopyVal location =<< toTypeVal (typeof location) (mkBool True)
            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i a
                valB <- tupleIdx i b
                equal <- newVal Bool

                storeInfix equal AST.EqEq valA valB
                cond <- freshName "tuple_eqeq_fail"
                e <- pload equal
                condBr (op e) (cases !! (i + 1)) cond
                emitBlockStart cond
                storeCopyVal location =<< toTypeVal (typeof location) (mkBool False)
                br exit

            emitBlockStart exit

        _ | operator `elem` [AST.GTEq] -> do
            deflt <- case operator of
                AST.GTEq -> return True
            storeCopyVal location =<< toTypeVal (typeof location) (mkBool deflt)

            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i a
                valB <- tupleIdx i b
                equal <- newVal Bool
                storeInfix equal AST.EqEq valA valB
                cond <- freshName "tuple_gt_cond"
                eq <- pload equal
                condBr (op eq) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                storeInfix location operator valA valB
                br exit

            emitBlockStart exit

        _ -> error (show operator)

tupleInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
tupleInfix operator a b = withErrorPrefix "tuple infix: " $ do
    assert (typeof a == typeof b) "type mismatch"
    Tuple ts <- baseTypeOf a

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            tup <- newVal (typeof a)
            forM (zip ts [0..]) $ \(t, i) -> do
                pSrcA <- tupleIdx i a
                pSrcB <- tupleIdx i b
                pDst  <- tupleIdx i tup
                storeInfix pDst operator pSrcA pSrcB
            return tup

        _ | operator `elem` [AST.EqEq] -> do
            res <- newBool True
            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i a
                valB <- tupleIdx i b
                equal <- newVal Bool
                storeInfix equal AST.EqEq valA valB
                cond <- freshName "tuple_eqeq_fail"
                eq <- pload equal
                condBr (op eq) (cases !! (i + 1)) cond
                emitBlockStart cond
                storeCopyVal res (mkBool False)
                br exit

            emitBlockStart exit
            return res

        _ | operator `elem` [AST.GTEq] -> do
            deflt <- case operator of
                AST.GTEq -> return True
            res <- newBool deflt

            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- tupleIdx i a
                valB <- tupleIdx i b
                equal <- newVal Bool
                storeInfix equal AST.EqEq valA valB
                cond <- freshName "tuple_gt_cond"
                eq <- pload equal
                condBr (op eq) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                storeInfix res operator valA valB
                br exit

            emitBlockStart exit
            return res

        _ -> error (show operator)
                    
        
storeTableInfix :: InsCmp CompileState m => Pointer -> AST.Operator -> Pointer -> Pointer -> m ()
storeTableInfix location operator a b = do
    Table ts <- baseTypeOf a
    noneDataType <- all (== False) <$> mapM isDataType ts

    assert (typeof a == typeof b) "type mismatch"

    lenA <- pload =<< tableLen a
    lenB <- tableLen b
    lenEq <- intInfix AST.EqEq lenA =<< pload lenB
    exit <- freshName "eqeq_table_exit"
    start <- freshName "eqeq_table_start"
    cond <- freshName "eqeq_table_cond"
    body <- freshName "eqeq_table_body"


    case operator of
        AST.NotEq -> do
            storeTableInfix location AST.EqEq a b
            storeCopyVal    location =<< prefix AST.Not location
        AST.EqEq  -> do
            storeCopyVal location (mkBool False)

            -- test that len(a) == len(b)
            condBr (op lenEq) start exit

            case noneDataType of 
                True -> do
                    emitBlockStart start

                    bs <- forM (zip ts [0..]) $ \(t, i) -> do
                        rowA <- tableRow i a
                        rowB <- tableRow i b
                        intInfix AST.EqEq (mkI64 0) =<< memCmp rowA rowB lenA
                    storeCopyVal location =<< foldM (boolInfix AST.AndAnd) (mkBool True) bs
                    br exit
                False -> do
                    emitBlockStart start
                    idx <- newI64 0
                    br cond

                    -- test that the idx < len
                    emitBlockStart cond
                    idxLT <- intInfix AST.GT lenA =<< pload idx
                    condBr (op idxLT) body exit

                    -- test that a[i] == b[i]
                    emitBlockStart body
                    columnA <- tableColumn a =<< pload idx
                    columnB <- tableColumn b =<< pload idx

                    storeCopyVal location (mkBool True)
                    forM_ (zip columnA columnB) $ \(elmA, elmB) -> do
                        oldEq <- pload location
                        storeInfix location AST.EqEq elmA elmB
                        storeCopyVal location =<< boolInfix AST.AndAnd oldEq =<< pload location

                    elemEq <- pload location
                    increment idx
                    condBr (op elemEq) cond exit

            emitBlockStart exit


storeAdtInfix :: InsCmp CompileState m => Pointer -> AST.Operator -> Pointer -> Pointer -> m ()
storeAdtInfix location operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    ADT fs <- baseTypeOf a
    case operator of
        --AST.NotEq -> fromValue <$> (prefix AST.Not . toValue =<< adtNormalInfix AST.EqEq a b)
        AST.EqEq -> do
            enA <- adtEnum =<< pload a
            enB <- adtEnum =<< pload b
            enEq <- intInfix AST.EqEq enA enB

            -- if enum isn't matched, exit
            storeCopyVal location (mkBool False)
            start <- freshName "start"
            exit <- freshName "exit"
            condBr (op enEq) start exit

            -- enum matched, match args
            emitBlockStart start
            storeCopyVal location (mkBool True)

            -- select block based on enum
            caseNames <- replicateM (length fs) (freshName "case")
            switch (op enA) exit $ zip (map (toCons . int64) [0..]) caseNames

            forM_ (zip caseNames [0..]) $ \(caseName, i) -> do
                emitBlockStart caseName
                case fs !! i of
                    FieldNull -> storeBasicVal location (mkBool True)
                    FieldType t -> do
                        valA <- adtField a i
                        valB <- adtField b i
                        storeInfix location AST.EqEq valA valB
                    FieldCtor ts -> do
                        valA <- adtField a i
                        valB <- adtField b i
                        storeInfix location AST.EqEq valA valB
                br exit

            emitBlockStart exit

