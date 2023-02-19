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

        Table ts -> do 
            len <- pload =<< tableLen src
            tableResize dst len
            for (op len) $ \n -> do 
                columnDst <- tableColumn dst (Value2 I64 n)
                columnSrc <- tableColumn src (Value2 I64 n)
                zipWithM_ storeCopy columnDst columnSrc

        Tuple ts -> do 
            forM_ [0..length ts - 1] $ \i -> do 
                dstElem <- tupleIdx i dst
                srcElem <- tupleIdx i src
                storeCopy dstElem srcElem

        _ -> fail (show baseDst)


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


mapFind :: InsCmp CompileState m => Pointer -> Pointer -> m (Value2, Value2) --found, idx
mapFind map key = do
    Map tk tv <- baseTypeOf map
    exit <- freshName "mapFind_exit"
    cond <- freshName "mapFind_cond"
    eqeq <- freshName "mapFind_EqEq"
    incr <- freshName "mapFind_incr"

    len <- pload =<< mapLen map
    idx <- newI64 0
    keys <- mapKeys map -- keyPtr
    found <- newBool False
    br cond

    emitBlockStart cond
    idxLT <- intInfix AST.GT len =<< pload idx
    condBr (op idxLT) eqeq exit

    emitBlockStart eqeq
    elm <- advancePointer keys =<< pload idx
    eq <- pload =<< newInfix AST.EqEq elm key
    storeCopyVal found eq
    condBr (op eq) exit incr

    emitBlockStart incr
    storeCopyVal idx =<< intInfix AST.Plus (mkI64 1) =<< pload idx
    br cond

    emitBlockStart exit
    f <- pload found
    i <- pload idx 
    return (f, i)



storeCopyVal :: InsCmp CompileState m => Pointer -> Value2 -> m ()
storeCopyVal dst src = do
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



prefix :: InsCmp CompileState m => AST.Operator -> Pointer -> m Value2
prefix operator val = do
    base <- baseTypeOf val
    Value2 (typeof val) <$> case base of
        _ | isInt base -> case operator of
            AST.Plus -> op <$> pload val
            AST.Minus -> do
                zero <- newVal (typeof val)
                op <$> (pload =<< newInfix AST.Minus zero val)

        _ | isFloat base -> case operator of
            AST.Plus -> op <$> pload val
            AST.Minus -> do 
                newFloat (typeof val) 0 >>= toVal >>= \zero -> fsub (valOp zero) . op =<< pload val

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
    base <- baseTypeOf a
    cnd <- pload =<< newInfix AST.GT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- pload a 
            valB <- pload b
            store (loc val) 0 =<< select (op cnd) (op valB) (op valA)
    return val


max :: InsCmp CompileState m => Pointer -> Pointer -> m Pointer
max a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    base <- baseTypeOf a
    cnd <- pload =<< newInfix AST.LT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- pload a 
            valB <- pload b
            store (loc val) 0 =<< select (op cnd) (op valB) (op valA)
    return val


-- any infix expression
newInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
newInfix operator a b = withErrorPrefix "infix: " $ do
    assert (typeof a == typeof b) $ "type mismatch: " ++ show (typeof a, typeof b)
    base <- baseTypeOf a
    case base of
        Bool                 -> do 
            av <- pload a
            res <- boolInfix operator av =<< pload b
            ret <- newVal (typeof res)
            storeCopyVal ret res 
            return ret
        Char                 -> do 
            av <- pload a
            res <- intInfix operator av =<< pload b
            ret <- newVal (typeof res)
            storeCopyVal ret res 
            return ret
        Enum -> do 
            av <- pload a
            bv <- pload b
            res <- enumInfix operator av bv
            ret <- newVal (typeof res)
            storeCopyVal ret res 
            return ret
        _ | isRange base     -> rangeInfix operator a b
        _ | isInt base       -> do 
            av <- pload a
            res <- intInfix operator av =<< pload b
            ret <- newVal (typeof res)
            storeCopyVal ret res 
            return ret
        _ | isFloat base     -> do 
            av <- pload a
            res <- floatInfix operator av =<< pload b
            ret <- newVal (typeof res)
            storeCopyVal ret res 
            return ret
        _ | isTuple base     -> tupleInfix operator a b
        _ | isArray base     -> arrayInfix operator a b
        _ | isTable base     -> tableInfix operator a b
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
            endA   <- rangeEnd a
            startEq <- pload =<< newInfix AST.EqEq startA =<< rangeStart b
            endEq   <- pload =<< newInfix AST.EqEq endA =<< rangeEnd b
            storeCopyVal val =<< boolInfix AST.AndAnd startEq endEq
            return val


arrayInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
arrayInfix operator a b = do
    assert (typeof a == typeof b) "infix type mismatch"
    Array n t <- assertBaseType isArray (typeof a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            arr <- newVal (typeof a)
            forM_ [0..n] $ \i -> do
                pDst <- arrayGetElem arr (mkI64 i)
                pSrcA <- arrayGetElem a (mkI64 i)
                pSrcB <- arrayGetElem b (mkI64 i)
                storeCopy pDst =<< newInfix operator pSrcA pSrcB
            return arr


tupleInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
tupleInfix operator a b = withErrorPrefix "tuple infix: " $ do
    assert (typeof a == typeof b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (typeof a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            tup <- newVal (typeof a)
            forM (zip ts [0..]) $ \(t, i) -> do
                pSrcA <- tupleIdx i a
                pSrcB <- tupleIdx i b
                pDst  <- tupleIdx i tup
                storeCopy pDst =<< newInfix operator pSrcA pSrcB
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
                equal <- pload =<< newInfix AST.EqEq valA valB
                cond <- freshName "tuple_eqeq_fail"
                condBr (op equal) (cases !! (i + 1)) cond
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
                equal <- pload =<< newInfix AST.EqEq valA valB
                cond <- freshName "tuple_gt_cond"
                condBr (op equal) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                storeCopy res =<< newInfix operator valA valB
                br exit

            emitBlockStart exit
            return res

        _ -> error (show operator)
                    
        
tableInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
tableInfix operator a b = do
    Table ts <- baseTypeOf (typeof a)
    assert (typeof a == typeof b) "type mismatch"

    lenA <- pload =<< tableLen a
    lenB <- tableLen b
    lenEq <- intInfix AST.EqEq lenA =<< pload lenB

    case operator of
        AST.NotEq -> do 
            res <- prefix AST.Not =<< tableInfix AST.EqEq a b
            ret <- newVal (typeof res) 
            storeCopyVal ret res 
            return ret

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
            storeCopyVal eq (mkBool True)
            br cond

            -- test that the idx < len
            emitBlockStart cond
            idxLT <- intInfix AST.GT lenA =<< pload idx
            condBr (op idxLT) body exit

            -- test that a[i] == b[i]
            emitBlockStart body
            columnA <- tableColumn a =<< pload idx
            columnB <- tableColumn b =<< pload idx
            eqs <- mapM pload =<< zipWithM (newInfix AST.EqEq) columnA columnB
            elemEq <- foldM (boolInfix AST.AndAnd) (mkBool True) eqs
            storeCopyVal eq elemEq
            storeCopyVal idx =<< intInfix AST.Plus (mkI64 1) =<< pload idx
            condBr (op elemEq) cond exit

            emitBlockStart exit
            return eq


valAdtNormalInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Value
valAdtNormalInfix operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    ADT fs <- assertBaseType isADT (typeof a)
    case operator of
        --AST.NotEq -> fromValue <$> (prefix AST.Not . toValue =<< valAdtNormalInfix AST.EqEq a b)
        AST.EqEq -> do
            enA <- adtEnum =<< pload a
            enB <- adtEnum =<< pload b
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
                        valA <- adtDeref (fromPointer a) i 0
                        valB <- adtDeref (fromPointer b) i 0
                        fmap (\a -> [a]) $ pload =<< newInfix AST.EqEq valA valB
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref (fromPointer a) i j
                            valB <- adtDeref (fromPointer b) i j
                            pload =<< newInfix AST.EqEq valA valB

                true <- pload =<< newBool True
                storeBasicVal match =<< foldM (boolInfix AST.EqEq) true bs
                br exit

            emitBlockStart exit
            toVal match

