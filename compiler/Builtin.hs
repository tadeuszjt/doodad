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


valsInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valsInfix operator a b = do
    Val typ opA  <- valLoad a
    Val typB opB <- valLoad b

    assert (typ == typB) "type mismatch"
    base <- baseTypeOf typ

    case base of
        Bool               -> boolInfix typ operator opA opB
        Char               -> valIntInfix operator a b
        _ | isInt base     -> valIntInfix operator a b
        _ | isFloat base   -> floatInfix typ operator opA opB
        _ | isEnumADT base -> adtEnumInfix typ operator opA opB
        _ | isTable base   -> tableInfix operator a b
        _ | isTuple base   -> tupleInfix operator a b
        _                  -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show typ ++ " " ++ show (valType b)

    where 
        boolInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        boolInfix typ operator opA opB = case operator of
            S.OrOr   -> Val typ <$> or opA opB
            S.AndAnd -> Val typ <$> and opA opB
            S.EqEq   -> Val typ <$> icmp P.EQ opA opB
            _        -> error ("bool infix: " ++ show operator)
        
        floatInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        floatInfix typ operator opA opB = case operator of
            S.Plus   -> Val typ <$> fadd opA opB
            S.Minus  -> Val typ <$> fsub opA opB
            S.Times  -> Val typ <$> fmul opA opB
            S.Divide -> Val typ <$> fdiv opA opB
            S.EqEq   -> Val Bool <$> fcmp P.OEQ opA opB
            _        -> error ("float infix: " ++ show operator)

        adtEnumInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        adtEnumInfix typ operator opA opB = case operator of
            S.NotEq -> Val Bool <$> icmp P.NE opA opB
            S.EqEq  -> Val Bool <$> icmp P.EQ opA opB

tupleInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
tupleInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (valType a)

    case operator of
        S.NotEq -> valNot =<< tupleInfix S.EqEq a b
        S.EqEq -> do
            bs <- forM (zip ts [0..]) $ \(t, i) -> do
                elmA <- tupleIdx i a
                elmB <- tupleIdx i b
                valsInfix S.EqEq elmA elmB

            true <- valBool Bool True
            foldM (valsInfix S.AndAnd) true bs
                    
        
tableInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
tableInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isTable (valType a)
    let typ = valType a

    lenA <- tableLen a
    lenB <- tableLen b
    lenEq <- valIntInfix S.EqEq lenA lenB

    case operator of
        S.NotEq -> valNot =<< tableInfix S.EqEq a b
        S.EqEq  -> do
            eq <- valLocal Bool
            idx <- valLocal I64
            valStore eq =<< valBool Bool False
            valStore idx =<< valInt I64 0

            exit <- freshName "eqeq_table_exit"
            start <- freshName "eqeq_table_start"
            cond <- freshName "eqeq_table_cond"
            body <- freshName "eqeq_table_body"

            -- test that len(a) == len(b)
            condBr (valOp lenEq) start exit
            emitBlockStart start
            valStore eq =<< valBool Bool True
            br cond

            -- test that the idx < len
            emitBlockStart cond
            idxLT <- valIntInfix S.LT idx lenA
            condBr (valOp idxLT) body exit

            -- test that a[i] == b[i]
            emitBlockStart body
            elmA <- tableGetElem a idx
            elmB <- tableGetElem b idx
            elmEq <- valsInfix S.EqEq elmA elmB
            valStore eq elmEq
            valStore idx =<< valIntInfix S.Plus idx (valI64 1)
            condBr (valOp elmEq) cond exit

            emitBlockStart exit
            valLoad eq
