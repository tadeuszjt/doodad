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


-- Builtin contains functions that operate on Values which require the
-- specialised Value functions such as Table, Array, Tuple etc. 


-- guarantees an equivalent value in a different memory location
storeCopy :: InsCmp CompileState m => Value -> Value -> m ()
storeCopy ptr val = withErrorPrefix "storeCopy: " $ do
    assert (isPtr ptr) "ptr isnt pointer"
    base <- baseTypeOf (typeof ptr)
    baseVal <- baseTypeOf (typeof val)
    assert (base == baseVal) $
        "ptr type: " ++ show (typeof ptr) ++ " does not match val type: " ++ show (typeof val)
    case base of
        _ | isSimple base -> valStore ptr val
        Tuple ts -> do
            bs <- mapM isDataType ts
            assert (all (==False) bs) "no data types allowed"
            valStore ptr val
        ADT fs -> do
            assertBaseType isADT (typeof ptr)
            isDataType <- isDataType (typeof ptr)
            assert (not isDataType) "ptr is data type"
            valStore ptr val
            

        _ -> fail (show base)


-- construct a value from arguments, Eg. i64(3.2), Vec2(12, 43)
mkConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
mkConstruct typ []       = mkZero typ
mkConstruct typ (a:b:xs) = mkTupleConstruct typ (a:b:xs)
mkConstruct typ [val]    = do
    base <- baseTypeOf typ
    case base of
        _ | isIntegral base -> fmap fromValue $ convertNumber typ . toValue =<< valLoad val
        _ | isFloat base    -> fmap fromValue $ convertNumber typ . toValue =<< valLoad val


-- constuct a tuple from the arguments. Eg. Vec2(1, 2)
mkTupleConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
mkTupleConstruct typ vals = trace "tupleConstruct" $ do
    Tuple ts <- assertBaseType isTuple typ
    tup <- newVal typ
    assert (length vals == length ts) "tuple length mismatch"
    forM_ (zip vals [0..]) $ \(val, i) -> do
        ptr <- tupleIdx i tup
        storeCopy (fromPointer ptr) val
    return (fromPointer tup)


-- convert the value into a new value corresponding to the type
mkConvert :: InsCmp CompileState m => Type -> Value -> m Value
mkConvert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf (typeof val)
    case base of
        _ | isIntegral base -> fmap fromValue $ convertNumber typ . toValue =<< valLoad val
        _ | isFloat base    -> fmap fromValue $ convertNumber typ . toValue =<< valLoad val
        _ | baseVal == base -> do
            ptr <- newVal typ
            storeCopy (fromPointer ptr) val
            return (fromPointer ptr)
        _ -> fail ("valConvert " ++ show base)



mkMin :: InsCmp CompileState m => Value -> Value -> m Value
mkMin a b = withErrorPrefix "min: " $ do
    assert (typeof a == typeof b) "Left type does not match right type"
    base <- baseTypeOf (typeof a)
    cnd <- mkInfix AST.GT a b 
    Val (typeof a) <$> case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            select (valOp cnd) (valOp valB) (valOp valA)


mkMax :: InsCmp CompileState m => Value -> Value -> m Value
mkMax a b = withErrorPrefix "min: " $ do
    assert (typeof a == typeof b) "Left type does not match right type"
    base <- baseTypeOf (typeof a)
    cnd <- mkInfix AST.LT a b 
    Val (typeof a) <$> case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            select (valOp cnd) (valOp valB) (valOp valA)


-- any infix expression
mkInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkInfix operator a b = withErrorPrefix "infix: " $ do
    assert (typeof a == typeof b) "type mismatch"
    base <- baseTypeOf (typeof a)
    case base of
        Bool                 -> do 
            av <- toValue <$> valLoad a
            bv <- toValue <$> valLoad b
            fromValue <$> boolInfix operator av bv
        Char                 -> do 
            av <- toValue <$> valLoad a
            fromValue <$> (intInfix operator av . toValue =<< valLoad b)
        Enum                 -> do 
            av <- toValue <$> valLoad a
            bv <- toValue <$> valLoad b
            fromValue <$> enumInfix operator av bv
        _ | isRange base     -> mkRangeInfix operator a b
        _ | isInt base       -> do 
            av <- toValue <$> valLoad a
            fromValue <$> (intInfix operator av . toValue =<< valLoad b)
        _ | isFloat base     -> do 
            av <- toValue <$> valLoad a
            fromValue <$> (floatInfix operator av . toValue =<< valLoad b)
        _ | isTuple base     -> mkTupleInfix operator a b
        _ | isArray base     -> mkArrayInfix operator a b
        _ | isTable base     -> mkTableInfix operator a b
        _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show (typeof a) ++ " " ++ show (typeof b)



enumInfix :: InsCmp CompileState m => AST.Operator -> Value2 -> Value2 -> m Value2
enumInfix operator a b = do
    assert (typeof a == typeof b) "type mismatch"
    Enum <- baseTypeOf (typeof a)
    case operator of
        AST.NotEq -> Value2 Bool <$> icmp P.NE (op a) (op b)
        AST.EqEq  -> Value2 Bool <$> icmp P.EQ (op a) (op b)


mkRangeInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkRangeInfix operator a b = do
    assertBaseType isRange (typeof a)
    assertBaseType isRange (typeof b)
    assert (typeof a == typeof b) "types do not match for range infix"
    case operator of
        AST.EqEq -> do
            startA <- fmap fromValue . pload =<< rangeStart (toPointer a)
            startB <- fmap fromValue . pload =<< rangeStart (toPointer b)
            endA   <- fmap fromValue . pload =<< rangeEnd (toPointer a)
            endB   <- fmap fromValue . pload =<< rangeEnd (toPointer b)
            startEq <- mkInfix AST.EqEq startA startB
            endEq   <- mkInfix AST.EqEq endA endB
            Val Bool <$> and (valOp startEq) (valOp endEq)


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
                valStore pDst =<< mkInfix operator pSrcA pSrcB
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
                valStore (fromPointer pDst) =<< mkInfix operator (fromPointer pSrcA) (fromPointer pSrcB)
            return (fromPointer tup)

        _ | operator `elem` [AST.EqEq] -> do
            res <- newBool True
            exit <- freshName "tuple_gt_exit"
            cases <- (\xs -> xs ++ [exit]) <$> replicateM (length ts) (freshName "tuple_gt_case")
            br (cases !! 0)

            forM (zip ts [0..]) $ \(t, i) -> do
                emitBlockStart (cases !! i)
                valA <- pload =<< tupleIdx i (toPointer a)
                valA <- pload =<< tupleIdx i (toPointer b)
                equal <- mkInfix AST.EqEq (fromValue valA) (fromValue valA)
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
                valA <- pload =<< tupleIdx i (toPointer a)
                valB <- pload =<< tupleIdx i (toPointer b)
                equal <- mkInfix AST.EqEq (fromValue valA) (fromValue valB)
                cond <- freshName "tuple_gt_cond"
                condBr (valOp equal) (cases !! (i + 1)) (cond)
                emitBlockStart cond
                valStore (fromPointer res) =<< mkInfix operator (fromValue valA) (fromValue valA)
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
            elmEq <- mkInfix AST.EqEq (fromPointer columnA) (fromPointer columnB)
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
            enA <- mkAdtEnum a
            enB <- mkAdtEnum b
            enAv <- toValue <$> valLoad enA
            enEq <- intInfix AST.EqEq enAv . toValue =<< valLoad enB

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
            switch (valOp enA) exit $ zip (map (toCons . int64) [0..]) caseNames

            forM_ (zip caseNames [0..]) $ \(caseName, i) -> do
                emitBlockStart caseName

                bs <- case fs !! i of
                    FieldNull -> fmap (\a -> [a]) $ toVal =<< newBool True
                    FieldType t -> do
                        valA <- adtDeref a i 0
                        valB <- adtDeref b i 0
                        fmap (\a -> [a]) $ mkInfix AST.EqEq valA valB
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref a i j
                            valB <- adtDeref b i j
                            mkInfix AST.EqEq valA valB

                true <- newBool True
                valStore (fromPointer match) =<< foldM (mkInfix AST.EqEq) (fromPointer true) bs
                br exit

            emitBlockStart exit
            toVal match

