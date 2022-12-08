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
import LLVM.AST.Global
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


-- Builtin contains functions that operate on Values which require the
-- specialised Value functions such as Table, Array, Tuple etc. 


-- guarantees an equivalent value in a different memory location
storeCopy :: InsCmp CompileState m => Value -> Value -> m ()
storeCopy ptr val = withErrorPrefix "storeCopy: " $ do
    assert (isPtr ptr) "ptr isnt pointer"
    base <- baseTypeOf (valType ptr)
    baseVal <- baseTypeOf (valType val)
    assert (base == baseVal) $
        "ptr type: " ++ show (valType ptr) ++ " does not match val type: " ++ show (valType val)
    case base of
        _ | isSimple base          -> valStore ptr val
        _ | isEnumADT base         -> valStore ptr val
        Tuple ts -> do
            bs <- mapM isDataType ts
            assert (all (==False) bs) "no data types allowed"
            valStore ptr val
            

        _ -> fail (show base)


-- construct a value from arguments, Eg. i64(3.2), Vec2(12, 43)
mkConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
mkConstruct typ []       = mkZero typ
mkConstruct typ (a:b:xs) = mkTupleConstruct typ (a:b:xs)
mkConstruct typ [val]    = do
    base <- baseTypeOf typ
    case base of
        _ | isIntegral base -> mkConvertNumber typ val
        _ | isFloat base    -> mkConvertNumber typ val


-- constuct a tuple from the arguments. Eg. Vec2(1, 2)
mkTupleConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
mkTupleConstruct typ vals = trace "tupleConstruct" $ do
    Tuple ts <- assertBaseType isTuple typ
    tup <- mkAlloca typ
    assert (length vals == length ts) "tuple length mismatch"
    forM_ (zip vals [0..]) $ \(val, i) -> do
        ptr <- ptrTupleIdx i tup
        storeCopy ptr val
    return tup


-- convert the value into a new value corresponding to the type
mkConvert :: InsCmp CompileState m => Type -> Value -> m Value
mkConvert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf (valType val)
    case base of
        _ | isIntegral base -> mkConvertNumber typ val
        _ | isFloat base    -> mkConvertNumber typ val
        _ | baseVal == base -> do
            ptr <- mkAlloca typ
            storeCopy ptr val
            return ptr
        _ -> fail ("valConvert " ++ show base)



mkMin :: InsCmp CompileState m => Value -> Value -> m Value
mkMin a b = withErrorPrefix "min: " $ do
    assert (valType a == valType b) "Left type does not match right type"
    base <- baseTypeOf (valType a)
    cnd <- mkInfix AST.GT a b 
    Val (valType a) <$> case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            select (valOp cnd) (valOp valB) (valOp valA)


mkMax :: InsCmp CompileState m => Value -> Value -> m Value
mkMax a b = withErrorPrefix "min: " $ do
    assert (valType a == valType b) "Left type does not match right type"
    base <- baseTypeOf (valType a)
    cnd <- mkInfix AST.LT a b 
    Val (valType a) <$> case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            select (valOp cnd) (valOp valB) (valOp valA)


-- any infix expression
mkInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkInfix operator a b = withErrorPrefix "infix: " $ do
    assert (valType a == valType b) "type mismatch"
    base <- baseTypeOf (valType a)
    case base of
        Bool                 -> mkBoolInfix operator a b
        Char                 -> mkIntInfix operator a b
        String               -> mkStringInfix operator a b
        _ | isEnumADT base   -> mkAdtEnumInfix operator a b
        _ | isInt base       -> mkIntInfix operator a b
        _ | isFloat base     -> mkFloatInfix operator a b
        _ | isTuple base     -> mkTupleInfix operator a b
        _ | isArray base     -> mkArrayInfix operator a b
        _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show (valType a) ++ " " ++ show (valType b)



mkStringInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkStringInfix operator a b = do
    assertBaseType (== String) (valType a)
    assertBaseType (== String) (valType b)
    case operator of
        AST.EqEq -> do
            locA <- valOp <$> valLoad a
            locB <- valOp <$> valLoad b
            i <- Val I64 <$> strcmp locA locB
            mkIntInfix AST.EqEq i (mkI64 0)


mkArrayInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkArrayInfix operator a b = withErrorPrefix "array" $ do
    assert (valType a == valType b) "infix type mismatch"
    Array n t <- assertBaseType isArray (valType a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            arr <- mkAlloca (valType a)
            forM_ [0..n] $ \i -> do
                pDst <- ptrArrayGetElemConst arr i
                pSrcA <- ptrArrayGetElemConst a i
                pSrcB <- ptrArrayGetElemConst b i
                valStore pDst =<< mkInfix operator pSrcA pSrcB
            return arr


mkTupleInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkTupleInfix operator a b = withErrorPrefix "tuple infix: " $ do
    assert (valType a == valType b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (valType a)

    case operator of
        _ | operator `elem` [AST.Plus, AST.Minus, AST.Times, AST.Divide, AST.Modulo] -> do
            tup <- mkAlloca (valType a)
            forM (zip ts [0..]) $ \(t, i) -> do
                pSrcA <- ptrTupleIdx i a
                pSrcB <- ptrTupleIdx i b
                pDst  <- ptrTupleIdx i tup
                valStore pDst =<< mkInfix operator pSrcA pSrcB
            return tup

        _ | operator `elem` [AST.EqEq] -> do
            res <- mkAlloca Bool
            valStore res =<< mkBool Bool True
            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                pSrcA <- ptrTupleIdx i a
                pSrcB <- ptrTupleIdx i b
                equal <- mkInfix AST.EqEq pSrcA pSrcB
                cond <- freshName "tuple_eqeq_fail"
                condBr (valOp equal) (cases !! (i + 1)) cond
                emitBlockStart cond
                valStore res =<< mkBool Bool False
                br exit

            emitBlockStart exit
            valLoad res

        _ | operator `elem` [AST.GTEq] -> do
            deflt <- case operator of
                AST.GTEq -> return True
            res <- mkAlloca Bool
            valStore res =<< mkBool Bool deflt -- deflt if all equal

            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                pSrcA <- ptrTupleIdx i a
                pSrcB <- ptrTupleIdx i b
                equal <- mkInfix AST.EqEq pSrcA pSrcB
                cond <- freshName "tuple_gt_cond"
                condBr (valOp equal) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                valStore res =<< mkInfix operator pSrcA pSrcB
                br exit

            emitBlockStart exit
            valLoad res

        _ -> error (show operator)
                    
        
valTableInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
valTableInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isTable (valType a)
    let typ = valType a

    lenA <- mkTableLen a
    lenB <- mkTableLen b
    lenEq <- mkIntInfix AST.EqEq lenA lenB

    case operator of
        AST.NotEq -> mkPrefix AST.Not =<< valTableInfix AST.EqEq a b
        AST.EqEq  -> do
            eq <- mkAlloca Bool
            idx <- mkAlloca I64
            valStore eq =<< mkBool Bool False
            valStore idx (mkI64 0)

            exit <- freshName "eqeq_table_exit"
            start <- freshName "eqeq_table_start"
            cond <- freshName "eqeq_table_cond"
            body <- freshName "eqeq_table_body"

            -- test that len(a) == len(b)
            condBr (valOp lenEq) start exit
            emitBlockStart start
            valStore eq =<< mkBool Bool True
            br cond

            -- test that the idx < len
            emitBlockStart cond
            idxLT <- mkIntInfix AST.LT idx lenA
            condBr (valOp idxLT) body exit

            -- test that a[i] == b[i]
            emitBlockStart body
            [elmA] <- ptrsTableColumn a idx
            [elmB] <- ptrsTableColumn b idx
            elmEq <- mkInfix AST.EqEq elmA elmB
            valStore eq elmEq
            valStore idx =<< mkIntInfix AST.Plus idx (mkI64 1)
            condBr (valOp elmEq) cond exit

            emitBlockStart exit
            valLoad eq


mkAdtEnumInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkAdtEnumInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isEnumADT (valType a)
    opA <- valOp <$> valLoad a
    opB <- valOp <$> valLoad b
    case operator of
        AST.NotEq -> Val Bool <$> icmp P.NE opA opB
        AST.EqEq  -> Val Bool <$> icmp P.EQ opA opB


valAdtNormalInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
valAdtNormalInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    base@(ADT fs) <- assertBaseType isNormalADT (valType a)

    case operator of
        AST.NotEq -> mkPrefix AST.Not =<< valAdtNormalInfix AST.EqEq a b
        AST.EqEq -> do
            enA <- valLoad =<< adtEnum a
            enB <- valLoad =<< adtEnum b
            enEq <- mkIntInfix AST.EqEq enA enB

            -- if enum isn't matched, exit
            match <- mkAlloca Bool
            valStore match =<< mkBool Bool False
            start <- freshName "start"
            exit <- freshName "exit"
            condBr (valOp enEq) start exit

            -- enum matched, match args
            emitBlockStart start
            valStore match =<< mkBool Bool True

            -- select block based on enum
            caseNames <- replicateM (length fs) (freshName "case")
            switch (valOp enA) exit $ zip (map (toCons . int64) [0..]) caseNames

            forM_ (zip caseNames [0..]) $ \(caseName, i) -> do
                emitBlockStart caseName

                bs <- case fs !! i of
                    FieldNull -> fmap (\a -> [a]) $ mkBool Bool True
                    FieldType t -> do
                        valA <- adtDeref a i 0
                        valB <- adtDeref b i 0
                        fmap (\a -> [a]) $ mkInfix AST.EqEq valA valB
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref a i j
                            valB <- adtDeref b i j
                            mkInfix AST.EqEq valA valB

                true <- mkBool Bool True
                valStore match =<< foldM (mkInfix AST.EqEq) true bs
                br exit

            emitBlockStart exit
            valLoad match

