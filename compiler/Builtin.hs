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

-- guarantees an equivalent value in a different memory location
valCopy :: InsCmp CompileState m => Value -> m Value
valCopy val = trace "valCopy" $ do
    base <- baseTypeOf (valType val)
    case base of
        _ | isSimple base -> valLoad val

        Table ts -> do
            len <- tableLen val
            tab <- tableMake (valType val) len

            forM_ (zip ts [0..]) $ \(t, i) -> do
                baseT <- baseTypeOf t
                valRow <- tableRow i val
                tabRow <- tableRow i tab

                if isSimple baseT
                then valMemCpy tabRow valRow len
                else for (valOp len) $ \op -> do
                    ptr <- valPtrIdx tabRow (Val I64 op)
                    valStore ptr =<< valCopy =<< valPtrIdx valRow (Val I64 op)

            valLoad tab

        Tuple ts -> do
            loc <- valLocal (valType val)
            forM_ (zip ts [0..]) $ \(t, i) ->
                tupleSet loc i =<< valCopy =<< tupleIdx i val
            valLoad loc

        _ -> fail $ "Can't handle copy: " ++ show (valType val)


valStringLen :: InsCmp CompileState m => Type -> Value -> m Value
valStringLen typ val = do
    assertBaseType (== I64) typ
    assertBaseType (== String) (valType val)
    op <- valOp <$> valLoad val
    n <- strlen op
    return $ Val typ n


--valString :: InsCmp CompileState m => Type -> Value -> m Value
--valString typ val = do
--    assertBaseType (== Table [Char]) typ
--    base <- baseTypeOf (valType val)
--    let bufSize = 32
--
--    resm <- lookm (Sym "string") $ KeyFunc [valType val] typ
--    valm <- case resm of
--        Just (ObjFunc op) -> do
--            valOpp <- valOp <$> valLoad val
--            fmap (Just . Val typ) (call op [(valOpp, [])])
--        Nothing           -> return Nothing
--
--    case valm of
--        Just v -> return v
--        Nothing -> do
--            tab <- tableMake typ (valI64 bufSize)
--            row <- tableRow 0 tab
--            ptr <- bitcast (valLoc row) (LL.ptr LL.i8)
--            op <- valOp <$> valLoad val
--
--            case base of
--                I64 -> do
--                    n <- Val I32 <$> snprintf ptr (int64 bufSize) "%d" [op]
--                    tableSetLen tab =<< valConvert I64 n
--                F64 -> do
--                    n <- Val I32 <$> snprintf ptr (int64 bufSize) "%f" [op]
--                    tableSetLen tab =<< valConvert I64 n
--                _ -> fail $ "No string function for: " ++ show (valType val)
--
--            valLoad tab


valConvert :: InsCmp CompileState m => Type -> Value -> m Value
valConvert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf (valType val)

    case base of
        _ | isInt base   -> valConvertNumber typ val
        _ | isFloat base -> valConvertNumber typ val
        Char             -> valConvertNumber typ val
        _ | baseVal == base -> case val of
            Ptr _ loc -> return $ Ptr typ loc
            Val _ op  -> return $ Val typ op
            

        --Table [Char]     -> valString typ val
        _                -> fail ("valConvert " ++ show base)



valZero :: InsCmp CompileState m => Type -> m Value
valZero typ = trace ("valZero " ++ show  typ) $ do
    namem <- case typ of
        Typedef symbol -> do
            ObType t nm <- look symbol KeyType
            return nm
        _ -> return Nothing

    base <- baseTypeOf typ
    case base of
        _ | isInt base     -> valInt typ 0
        _ | isFloat base   -> valFloat typ 0.0
        Bool               -> valBool typ False
        Char               -> valChar typ '\0'
        Array n t          -> Val typ . array . replicate n . toCons . valOp <$> valZero t
        Tuple ts           -> Val typ . struct namem True . map (toCons . valOp) <$> mapM valZero ts
        _ | isEnumADT base -> Val typ . valOp <$> valZero I64

        Table ts         -> do
            let zi64 = toCons (int64 0)
            zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
            return $ Val typ $ struct namem False (zi64:zi64:zptrs)

        ADT fs | isNormalADT base -> do
            im <- adtHasNull typ
            case im of
                Just i -> do
                    let ii64 = toCons (int64 $ fromIntegral i)
                    let zi64 = toCons (int64 0)
                    let zptr = C.IntToPtr zi64 (LL.ptr LL.void)
                    return $ Val typ $ struct namem False [ii64, zptr]

                Nothing -> do
                    case head fs of
                        FieldCtor [] -> do
                            let zi64 = toCons (int64 0)
                            let zptr = C.IntToPtr zi64 (LL.ptr LL.void)
                            return $ Val typ $ struct namem False [zi64, zptr]

                                
        _ -> error ("valZero: " ++  show typ)


valsInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valsInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    base <- baseTypeOf (valType a)

    if base == Void then nullInfix operator
    else do
        Val typ opA  <- valLoad a
        Val typB opB <- valLoad b

        case base of
            Bool                 -> boolInfix typ operator opA opB
            Char                 -> valIntInfix operator a b
            Void                 -> nullInfix operator
            _ | isInt base       -> valIntInfix operator a b
            _ | isFloat base     -> floatInfix typ operator opA opB
            _ | isTable base     -> valTableInfix operator a b
            _ | isTuple base     -> valTupleInfix operator a b
            _ | isNormalADT base -> valAdtNormalInfix operator a b
            _ | isEnumADT base   -> valAdtEnumInfix operator a b
            _                    -> fail $ "Operator " ++ show operator ++ " undefined for types " ++ show typ ++ " " ++ show (valType b)

    where 
        nullInfix :: InsCmp CompileState m => S.Op -> m Value
        nullInfix operator = case operator of
            S.EqEq -> valBool Bool True
            S.NotEq -> valBool Bool False

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


valAdtEnumInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valAdtEnumInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isEnumADT (valType a)
    opA <- valOp <$> valLoad a
    opB <- valOp <$> valLoad b
    case operator of
        S.NotEq -> Val Bool <$> icmp P.NE opA opB
        S.EqEq  -> Val Bool <$> icmp P.EQ opA opB


valAdtNormalInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valAdtNormalInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    base@(ADT fs) <- assertBaseType isNormalADT (valType a)

    case operator of
        S.NotEq -> valNot =<< valAdtNormalInfix S.EqEq a b
        S.EqEq -> do
            enA <- valLoad =<< adtEnum a
            enB <- valLoad =<< adtEnum b
            enEq <- valIntInfix S.EqEq enA enB

            -- if enum isn't matched, exit
            match <- valLocal Bool
            valStore match =<< valBool Bool False
            start <- freshName "start"
            exit <- freshName "exit"
            condBr (valOp enEq) start exit

            -- enum matched, match args
            emitBlockStart start
            valStore match =<< valBool Bool True

            -- select block based on enum
            caseNames <- replicateM (length fs) (freshName "case")
            switch (valOp enA) exit $ zip (map (toCons . int64) [0..]) caseNames

            forM_ (zip caseNames [0..]) $ \(caseName, i) -> do
                emitBlockStart caseName

                bs <- case fs !! i of
                    FieldNull -> fmap (\a -> [a]) $ valBool Bool True
                    FieldType t -> do
                        valA <- adtDeref a i 0
                        valB <- adtDeref b i 0
                        fmap (\a -> [a]) $ valsInfix S.EqEq valA valB
                    FieldCtor ts -> do
                        forM (zip ts [0..]) $ \(t, j) -> do
                            valA <- adtDeref a i j
                            valB <- adtDeref b i j
                            valsInfix S.EqEq valA valB

                true <- valBool Bool True
                valStore match =<< foldM (valsInfix S.EqEq) true bs
                br exit

            emitBlockStart exit
            valLoad match


valTupleInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valTupleInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    Tuple ts <- assertBaseType isTuple (valType a)

    case operator of
        S.NotEq -> valNot =<< valTupleInfix S.EqEq a b
        S.EqEq -> do
            bs <- forM (zip ts [0..]) $ \(t, i) -> do
                elmA <- tupleIdx i a
                elmB <- tupleIdx i b
                valsInfix S.EqEq elmA elmB

            true <- valBool Bool True
            foldM (valsInfix S.AndAnd) true bs
                    
        
valTableInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valTableInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType isTable (valType a)
    let typ = valType a

    lenA <- tableLen a
    lenB <- tableLen b
    lenEq <- valIntInfix S.EqEq lenA lenB

    case operator of
        S.NotEq -> valNot =<< valTableInfix S.EqEq a b
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
