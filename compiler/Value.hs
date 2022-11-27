{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Value where

import Prelude hiding (or, and)
import GHC.Float
import Data.Maybe
import Data.List hiding (or, and)
import Control.Monad
import Control.Monad.State hiding (void)
import Control.Monad.Trans

import LLVM.AST.Name
import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.Internal.EncodeAST
import LLVM.Internal.Coding hiding (alloca)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as P

import qualified AST as S
import Monad
import State
import Funcs
import Type
import Typeof
import Trace
import Error


-- Value contains the basic low-level operations for Value types.

mkZero :: InsCmp CompileState m => Type -> m Value
mkZero typ = trace ("mkZero " ++ show  typ) $ do
    namem <- case typ of
        Typedef symbol -> do
            ObType t nm <- look symbol KeyType
            return nm
        _ -> return Nothing

    base <- baseTypeOf typ
    case base of
        _ | isInt base     -> mkInt typ 0
        _ | isFloat base   -> mkFloat typ 0.0
        Bool               -> mkBool typ False
        Char               -> mkChar typ '\0'
        Array n t          -> Val typ . array . replicate n . toCons . valOp <$> mkZero t
        Tuple ts           -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero ts
        _ | isEnumADT base -> Val typ . valOp <$> mkZero I64

        Table ts         -> do
            let zi64 = toCons (int64 0)
            zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
            return $ Val typ $ struct namem False (zi64:zi64:zptrs)

        Sparse ts         -> do
            let zi64 = toCons (int64 0)
            let zpi64 = C.IntToPtr zi64 (LL.ptr LL.i64)
            zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
            let stack = toCons $ struct Nothing False [zi64,zi64,zpi64]
            let table = toCons $ struct Nothing False (zi64:zi64:zptrs)
            return $ Val typ $ struct namem False [table, stack]
                                
        _ -> error ("mkZero: " ++  show typ)


mkI64 :: Integral i => i -> Value
mkI64 n = Val I64 $ int64 (fromIntegral n)


mkChar :: InsCmp CompileState m => Type -> Char -> m Value
mkChar typ c = do
    assertBaseType (== Char) typ
    return $ Val typ (int8 $ fromIntegral $ fromEnum c)


mkBool :: InsCmp CompileState m => Type -> Bool -> m Value
mkBool typ b = do
    assertBaseType (== Bool) typ
    return $ Val typ (if b then bit 1 else bit 0)


mkInt :: InsCmp CompileState m => Integral i => Type -> i -> m Value
mkInt typ n = trace "mkInt" $ do
    base <- assertBaseType isInt typ
    return $ Val typ $ case base of
        I8  -> int8 (fromIntegral n)
        I32 -> int32 (fromIntegral n)
        I64 -> int64 (fromIntegral n)


mkFloat :: InsCmp CompileState m => Type -> Double -> m Value
mkFloat typ f = trace "mkFloat" $ do
    base <- assertBaseType isFloat typ
    return $ Val typ $ case base of
        F32 -> single (double2Float f)
        F64 -> double f


mkRange :: InsCmp CompileState m => Value -> Value -> m Value
mkRange start end = do
    assert (valType start == valType end) "range types do not match"
    assertBaseType isSimple (valType start) -- simple types so valLoad is mk

    range    <- valLocal $ Range (valType start)
    startDst <- ptrRangeStart range
    endDst   <- ptrRangeEnd range

    valStore startDst start
    valStore endDst end
    return range


ptrRangeStart :: InsCmp CompileState m => Value -> m Value
ptrRangeStart val = do
    assert (isPtr val) "val isn't pointer"
    Range t <- assertBaseType (isRange) (valType val)
    Ptr t <$> gep (valLoc val) [int32 0, int32 0]


ptrRangeEnd :: InsCmp CompileState m => Value -> m Value
ptrRangeEnd val = do
    assert (isPtr val) "val isn't pointer"
    Range t <- assertBaseType (isRange) (valType val)
    Ptr t <$> gep (valLoc val) [int32 0, int32 1]


mkStringLen :: InsCmp CompileState m => Type -> Value -> m Value
mkStringLen typ val = do
    assertBaseType (== I64) typ
    assertBaseType (== String) (valType val)
    op <- valOp <$> valLoad val
    Val typ <$> strlen op


mkConvertNumber :: InsCmp CompileState m => Type -> Value -> m Value
mkConvertNumber typ val = do
    op <- valOp <$> valLoad val
    base <- baseTypeOf typ
    baseVal <- baseTypeOf (valType val)
    fmap (Val typ) $ case (base, baseVal) of
        (I64,  I64) -> return op
        (I32,  I64) -> trunc op LL.i32
        (I16,  I64) -> trunc op LL.i16
        (I8,   I64) -> trunc op LL.i8
        (Char, I64) -> trunc op LL.i8
        (F64,  I64) -> sitofp op LL.double
        (F32,  I64) -> sitofp op LL.float

        (I64,  I32) -> sext op LL.i64
        (I32,  I32) -> return op
        (I16,  I32) -> trunc op LL.i16
        (I8,   I32) -> trunc op LL.i8
        (Char, I32) -> trunc op LL.i8

        (I64, I16) -> sext op LL.i64
        (I32, I16) -> sext op LL.i32
        (I16, I16) -> return op
        (I8,  I16) -> trunc op LL.i8
        (Char, I16) -> trunc op LL.i8

        (I64 , I8) -> sext op LL.i64
        (I32 , I8) -> sext op LL.i32
        (I16 , I8) -> sext op LL.i16
        (I8  , I8) -> return op
        (Char, I8) -> return op

        (I64,  Char) -> sext op LL.i64
        (I32,  Char) -> sext op LL.i32
        (I16,  Char) -> sext op LL.i16
        (I8,   Char) -> return op
        (Char, Char) -> return op

        (I64, F64) -> fptosi op LL.i64
        (F32, F64) -> fptrunc op LL.float
        (F64, F64) -> return op

        (F64, F32) -> fpext op LL.double


valLoad :: InsCmp s m => Value -> m Value
valLoad (Val typ op)  = trace ("valLoad " ++ show typ) $ return (Val typ op)
valLoad (Ptr typ loc) = trace ("valLoad " ++ show typ) $ Val typ <$> load loc 0


valStore :: InsCmp CompileState m => Value -> Value -> m ()
valStore (Ptr typ loc) val = trace "valStore" $ do
    base <- baseTypeOf typ
    valBase <- baseTypeOf (valType val)
    assert (base == valBase) "types incompatible"
    case val of
        Ptr t l -> store loc 0 =<< load l 0
        Val t o -> store loc 0 o


valSelect :: InsCmp CompileState m => Value -> Value -> Value -> m Value
valSelect cnd true false = trace "valSelect" $ do
    assertBaseType (==Bool) (valType cnd)
    assert (valType true == valType false) "incompatible types"

    cndOp <- valOp <$> valLoad cnd
    trueOp <- valOp <$> valLoad true
    falseOp <- valOp <$> valLoad false
    Val (valType true) <$> select cndOp trueOp falseOp


valLocal :: InsCmp CompileState m => Type -> m Value
valLocal typ = trace ("valLocal " ++ show typ) $ do
    opTyp <- opTypeOf typ
    Ptr typ <$> alloca opTyp Nothing 0
    

valMalloc :: InsCmp CompileState m => Type -> Value -> m Value
valMalloc typ len = trace ("valMalloc " ++ show typ) $ do
    lenTyp <- assertBaseType isInt (valType len)
    lenOp <- valOp <$> valLoad len
    pi8 <- malloc =<< mul lenOp . valOp =<< sizeOf typ
    fmap (Ptr typ) $ bitcast pi8 . LL.ptr =<< opTypeOf typ


valPtrIdx :: InsCmp CompileState m => Value -> Value -> m Value
valPtrIdx (Ptr typ loc) idx = trace ("valPtrIdx " ++ show typ) $ do
    assertBaseType (== I64) (valType idx)
    op <- valOp <$> valLoad idx
    Ptr typ <$> gep loc [op]


valStringIdx :: InsCmp CompileState m => Value -> Value -> m Value
valStringIdx str idx = do
    assertBaseType (== String) (valType str)
    assertBaseType isInt (valType idx)
    loc <- valOp <$> valLoad str
    op <- valOp <$> valLoad idx
    pi8 <- gep loc [op]
    Val Char <$> load pi8 0


valMemCpy :: InsCmp CompileState m => Value -> Value -> Value -> m ()
valMemCpy (Ptr dstTyp dst) (Ptr srcTyp src) len = trace "valMemCpy" $ do
    assert (dstTyp == srcTyp) "Types do not match"
    assertBaseType isInt (valType len)

    pDstI8 <- bitcast dst (LL.ptr LL.i8)
    pSrcI8 <- bitcast src (LL.ptr LL.i8)

    void $ memcpy pDstI8 pSrcI8 . valOp =<< mkIntInfix S.Times len =<< sizeOf dstTyp


valPrefix :: InsCmp CompileState m => S.Operator -> Value -> m Value
valPrefix operator val = do
    Val typ op <- valLoad val
    base <- baseTypeOf typ
    Val typ <$> case base of
        _ | isInt base -> case operator of
            S.Plus -> return op
            S.Minus -> mkInt typ 0 >>= \zero -> sub (valOp zero) op

        _ | isFloat base -> case operator of
            S.Plus -> return op
            S.Minus -> mkFloat typ 0 >>= \zero -> fsub (valOp zero) op

        Bool -> case operator of
            S.Not -> icmp P.EQ op (bit 0)
        

mkIntInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
mkIntInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType (\t -> isInt t || t == Char) (valType a)
    Val typ opA <- valLoad a
    Val _   opB <- valLoad b
    case operator of
        S.Plus   -> Val typ  <$> add opA opB
        S.Minus  -> Val typ  <$> sub opA opB
        S.Times  -> Val typ  <$> mul opA opB
        S.Divide -> Val typ  <$> sdiv opA opB
        S.Modulo -> Val typ  <$> srem opA opB
        S.GT     -> Val Bool <$> icmp P.SGT opA opB
        S.LT     -> Val Bool <$> icmp P.SLT opA opB
        S.GTEq   -> Val Bool <$> icmp P.SGE opA opB
        S.LTEq   -> Val Bool <$> icmp P.SLE opA opB
        S.EqEq   -> Val Bool <$> icmp P.EQ opA opB
        S.NotEq  -> Val Bool <$> icmp P.NE opA opB
        _        -> error ("int infix: " ++ show operator)
    
        
