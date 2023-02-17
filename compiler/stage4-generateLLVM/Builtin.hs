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
storeCopy :: InsCmp CompileState m => Pointer -> Pointer -> m ()
storeCopy dst src = withErrorPrefix "storeCopy: " $ do
    baseDst <- baseTypeOf dst
    baseSrc <- baseTypeOf src
    True <- return $ baseDst == baseSrc
    case baseDst of
        _ | isSimple baseDst -> storeBasic dst src
        _ -> fail (show baseDst)


storeCopyVal :: InsCmp CompileState m => Pointer -> Value2 -> m ()
storeCopyVal dst src = withErrorPrefix "storeCopy: " $ do
    baseDst <- baseTypeOf dst
    baseSrc <- baseTypeOf src
    True <- return $ baseDst == baseSrc
    case baseDst of
        _ | isSimple baseDst -> storeBasicVal dst src
        _ -> fail (show baseDst)


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
newConvert :: InsCmp CompileState m => Type -> Value -> m Pointer
newConvert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf val
    ptr <- newVal typ
    case base of
        _ | isIntegral base -> storeBasicVal ptr =<< convertNumber typ . toValue =<< valLoad val
        _ | isFloat base    -> storeBasicVal ptr =<< convertNumber typ . toValue =<< valLoad val
        _ | baseVal == base -> storeCopy ptr (toPointer val)
        _ -> fail ("valConvert " ++ show base)
    return ptr


min :: InsCmp CompileState m => Value -> Value -> m Pointer
min a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    base <- baseTypeOf a
    cnd <- mkInfix AST.GT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            store (loc val) 0 =<< select (valOp cnd) (valOp valB) (valOp valA)
    return val


max :: InsCmp CompileState m => Value -> Value -> m Pointer
max a b = withErrorPrefix "min: " $ do
    True <- return $ typeof a == typeof b
    base <- baseTypeOf a
    cnd <- mkInfix AST.LT a b 
    val <- newVal (typeof a)
    case base of
        I64 -> do
            valA <- valLoad a 
            valB <- valLoad b
            store (loc val) 0 =<< select (valOp cnd) (valOp valB) (valOp valA)
    return val


-- any infix expression
mkInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkInfix operator a b = withErrorPrefix "infix: " $ do
    assert (typeof a == typeof b) $ "type mismatch: " ++ show (typeof a, typeof b)
    base <- baseTypeOf a
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
        _ | isRange base     -> toVal =<< rangeInfix operator (toPointer a) (toPointer b)
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
            startEq <- mkInfix AST.EqEq (fromPointer startA) (fromPointer startB)
            endEq   <- mkInfix AST.EqEq (fromPointer endA) (fromPointer endB)
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

