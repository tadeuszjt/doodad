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


valI64 :: Integral i => i -> Value
valI64 n = Val I64 $ int64 (fromIntegral n)


valChar :: InsCmp CompileState m => Type -> Char -> m Value
valChar typ c = do
    assertBaseType (== Char) typ
    return $ Val typ (int8 $ fromIntegral $ fromEnum c)


valBool :: InsCmp CompileState m => Type -> Bool -> m Value
valBool typ b = do
    assertBaseType (== Bool) typ
    return $ Val typ (if b then bit 1 else bit 0)


valInt :: InsCmp CompileState m => Integral i => Type -> i -> m Value
valInt typ n = trace "valInt" $ do
    base <- assertBaseType isInt typ
    return $ case base of
        I8  -> Val typ $ int8 (fromIntegral n)
        I32 -> Val typ $ int32 (fromIntegral n)
        I64 -> Val typ $ int64 (fromIntegral n)


valFloat :: InsCmp CompileState m => Type -> Double -> m Value
valFloat typ f = trace "valFloat" $ do
    base <- assertBaseType isFloat typ
    return $ case base of
        F32 -> Val typ $ single (double2Float f)
        F64 -> Val typ $ double f



valConvertNumber :: InsCmp CompileState m => Type -> Value -> m Value
valConvertNumber typ val = do
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

    void $ memcpy pDstI8 pSrcI8 . valOp =<< valIntInfix S.Times len =<< sizeOf dstTyp


valNot :: InsCmp CompileState m => Value -> m Value
valNot val = trace "valNot" $ do
    assertBaseType (== Bool) (valType val)
    fmap (Val $ valType val) $ icmp P.EQ (bit 0) . valOp =<< valLoad val


valPrefix :: InsCmp CompileState m => S.Operator -> Value -> m Value
valPrefix operator val = do
    Val typ op <- valLoad val
    base <- baseTypeOf typ
    case base of
        _ | isInt base -> case operator of
            S.Minus -> do
                zero <- valInt typ 0
                Val typ <$> sub (valOp zero) op

            S.Plus -> return (Val typ op)

        _ | isFloat base -> case operator of
            S.Minus -> do
                zero <- valFloat typ 0
                Val typ <$> fsub (valOp zero) op

        Bool -> case operator of
            S.Not -> do
                false <- valBool typ False
                Val typ <$> icmp P.EQ (valOp false) op
        

valIntInfix :: InsCmp CompileState m => S.Operator -> Value -> Value -> m Value
valIntInfix operator a b = do
    assert (valType a == valType b) "type mismatch"
    assertBaseType (\t -> isInt t || t == Char) (valType a)
    Val typ opA <- valLoad a
    Val _   opB <- valLoad b
    case operator of
        S.Plus   -> Val typ  <$> add opA opB
        S.Minus  -> Val typ  <$> sub opA opB
        S.Times  -> Val typ  <$> mul opA opB
        S.Divide -> Val typ  <$> sdiv opA opB
        S.GT     -> Val Bool <$> icmp P.SGT opA opB
        S.LT     -> Val Bool <$> icmp P.SLT opA opB
        S.GTEq   -> Val Bool <$> icmp P.SGE opA opB
        S.LTEq   -> Val Bool <$> icmp P.SLE opA opB
        S.EqEq   -> Val Bool <$> icmp P.EQ opA opB
        S.NotEq  -> Val Bool <$> icmp P.NE opA opB
        S.Modulo -> Val typ  <$> srem opA opB
        _        -> error ("int infix: " ++ show operator)
    
        
