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

import qualified AST as S
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
storeCopy ptr val = do
    assert (isPtr ptr) "ptr isnt pointer"
    base <- baseTypeOf (valType ptr)
    baseVal <- baseTypeOf (valType val)
    assert (base == baseVal) "ptr type does not match val type"
    case base of
        _ | isSimple base -> valStore ptr val
        _ -> fail (show base)


-- construct a value from arguments, Eg. i64(3.2), Vec2(12, 43)
mkConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
mkConstruct typ []       = mkZero typ
mkConstruct typ (a:b:xs) = tupleConstruct typ (a:b:xs)
mkConstruct typ [val]    = do
    base <- baseTypeOf typ
    case base of
        _ | isIntegral base -> mkConvertNumber typ val
        _ | isFloat base    -> mkConvertNumber typ val


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


-- any infix expression
valsInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valsInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    base <- baseTypeOf (valType a)
    case base of
        Bool                 -> mkBoolInfix operator a b
        Char                 -> mkIntInfix operator a b
        _ | isEnumADT base   -> mkAdtEnumInfix operator a b
        _ | isInt base       -> mkIntInfix operator a b
        _ | isFloat base     -> mkFloatInfix operator a b
        _ | isArray base     -> valArrayInfix operator a b
        _ | isTable base     -> valTableInfix operator a b
        _ | isTuple base     -> valTupleInfix operator a b
        _ | isNormalADT base -> valAdtNormalInfix operator a b
        _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show (valType a) ++ " " ++ show (valType b)
    where 


valArrayInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valArrayInfix operator a b = withErrorPrefix "array" $ do
    assert (valType a == valType b) "infix type mismatch"
    Array n t <- assertBaseType isArray (valType a)

    case operator of
        _ | operator `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
            arr <- mkAlloca (valType a)
            forM_ [0..n] $ \i -> do
                pDst <- ptrArrayGetElemConst arr i
                pA <- ptrArrayGetElemConst a i
                pB <- ptrArrayGetElemConst b i
                valStore pDst =<< valsInfix operator pA pB
            return arr



valTupleInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valTupleInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (valType a)

    case operator of
        _ | operator `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
            tup <- mkAlloca (valType a)
            bs <- forM (zip ts [0..]) $ \(t, i) -> do
                pA <- ptrTupleIdx i a
                pB <- ptrTupleIdx i b
                pDst <- ptrTupleIdx i tup
                valStore pDst =<< valsInfix operator pA pB
            return tup

        S.NotEq -> mkPrefix S.Not =<< valTupleInfix S.EqEq a b
        S.EqEq -> do
            bs <- forM (zip ts [0..]) $ \(t, i) -> do
                elmA <- ptrTupleIdx i a
                elmB <- ptrTupleIdx i b
                valsInfix S.EqEq elmA elmB

            true <- mkBool Bool True
            foldM (valsInfix S.AndAnd) true bs
        _ -> error (show operator)
                    
        
valTableInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valTableInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isTable (valType a)
    let typ = valType a

    lenA <- tableLen a
    lenB <- tableLen b
    lenEq <- mkIntInfix S.EqEq lenA lenB

    case operator of
        S.NotEq -> mkPrefix S.Not =<< valTableInfix S.EqEq a b
        S.EqEq  -> do
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
            idxLT <- mkIntInfix S.LT idx lenA
            condBr (valOp idxLT) body exit

            -- test that a[i] == b[i]
            emitBlockStart body
            [elmA] <- tableGetColumn a idx
            [elmB] <- tableGetColumn b idx
            elmEq <- valsInfix S.EqEq elmA elmB
            valStore eq elmEq
            valStore idx =<< mkIntInfix S.Plus idx (mkI64 1)
            condBr (valOp elmEq) cond exit

            emitBlockStart exit
            valLoad eq


mkAdtEnumInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
mkAdtEnumInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isEnumADT (valType a)
    opA <- valOp <$> valLoad a
    opB <- valOp <$> valLoad b
    case operator of
        S.NotEq -> Val Bool <$> icmp P.NE opA opB
        S.EqEq  -> Val Bool <$> icmp P.EQ opA opB


valAdtNormalInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valAdtNormalInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    base@(ADT fs) <- assertBaseType isNormalADT (valType a)

    case operator of
        S.NotEq -> mkPrefix S.Not =<< valAdtNormalInfix S.EqEq a b
        S.EqEq -> do
            enA <- valLoad =<< adtEnum a
            enB <- valLoad =<< adtEnum b
            enEq <- mkIntInfix S.EqEq enA enB

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
                        fmap (\a -> [a]) $ valsInfix S.EqEq valA valB
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref a i j
                            valB <- adtDeref b i j
                            valsInfix S.EqEq valA valB

                true <- mkBool Bool True
                valStore match =<< foldM (valsInfix S.EqEq) true bs
                br exit

            emitBlockStart exit
            valLoad match

