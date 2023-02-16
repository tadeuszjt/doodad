{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Value where

import qualified Data.Map as Map
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

import qualified AST
import Monad
import State
import Funcs
import Type
import Typeof
import Trace
import Error


-- Value contains the basic low-level operations for Value types.

mkI64 :: Integral i => i -> Value
mkI64 n = Val I64 $ int64 (fromIntegral n)



toPointer (Ptr t p) = Pointer t p

fromPointer (Pointer t p) = Ptr t p


toVal :: InsCmp CompileState m => Pointer -> m Value 
toVal (Pointer t loc) = do 
    assertBaseType isSimple t
    valLoad (Ptr t loc)


toType :: InsCmp CompileState m => Type -> Pointer -> m Pointer
toType typ ptr = do 
    base <- baseTypeOf typ
    basePtr <- baseTypeOf (typeof ptr)
    assert (base == basePtr) "Incompatible types"
    return $ Pointer typ (loc ptr)


newI64 :: (InsCmp CompileState m, Integral i) => i -> m Pointer
newI64 n = do 
    p <- alloca LL.i64 Nothing 0
    store p 0 $ int64 (fromIntegral n)
    return $ Pointer I64 p


newChar :: InsCmp CompileState m => Char -> m Pointer
newChar c = do 
    p <- alloca LL.i8 Nothing 0
    store p 0 $ int8 (fromIntegral $ fromEnum c)
    return $ Pointer Char p


newBool :: InsCmp CompileState m => Bool -> m Pointer
newBool b = do 
    p <- alloca LL.i1 Nothing 0
    store p 0 $ if b then bit 1 else bit 0
    return $ Pointer Bool p


newFloat :: InsCmp CompileState m => Type -> Double -> m Pointer
newFloat typ f = do 
    base <- baseTypeOf typ 
    case base of 
        F64 -> do
            p <- alloca LL.double Nothing 0
            store p 0 $ double f
            return $ Pointer typ p
        F32 -> do
            p <- alloca LL.float Nothing 0
            store p 0 $ single (double2Float f)
            return $ Pointer typ p


mkInt :: InsCmp CompileState m => Integral i => Type -> i -> m Value
mkInt typ n = trace "mkInt" $ do
    base <- assertBaseType isInt typ
    return $ Val typ $ case base of
        I8  -> int8 (fromIntegral n)
        I32 -> int32 (fromIntegral n)
        I64 -> int64 (fromIntegral n)

mkEnum :: InsCmp CompileState m => Integral i => Type -> i -> m Value
mkEnum typ n = do
    assertBaseType (== Enum) typ
    return $ Val typ $ int64 (fromIntegral n)


mkRange :: InsCmp CompileState m => Value -> Value -> m Value
mkRange start end = do
    assert (valType start == valType end) $ "range types do not match"
    isDataType <- isDataType (valType start)
    assert (not isDataType) "simple types so valLoad is mk"

    range    <- mkAlloca $ Range (valType start) -- LEAVE THIS ONE FOR NOW
    startDst <- ptrRangeStart range
    endDst   <- ptrRangeEnd range

    valStore startDst start
    valStore endDst end
    return range




mkZero :: InsCmp CompileState m => Type -> m Value
mkZero typ = trace ("mkZero " ++ show  typ) $ do
    namem <- Map.lookup typ <$> gets typeNameMap
    base <- baseTypeOf typ
    case base of
        _ | isInt base     -> mkInt typ 0
        F32                -> return $ Val typ (single 0.0)
        F64                -> return $ Val typ (double 0.0)
        Enum               -> return $ Val typ (int64 0)
        Bool               -> return $ Val typ (bit 0)
        Char               -> return $ Val typ (int8 0)
        Array n t          -> Val typ . array . replicate n . toCons . valOp <$> mkZero t
        Tuple ts           -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero ts
        Table ts           -> Val typ . struct namem False . ([zi64, zi64] ++) <$> map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
        Sparse ts          -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero [Table ts, Table [I64]]
        Map tk tv          -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero [Table [tk], Sparse [tv]]
        _ -> error ("mkZero: " ++  show typ)
        where
            zi64 = toCons (int64 0)


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


mkRangeStart :: InsCmp CompileState m => Value -> m Value
mkRangeStart val = do
    Range t <- assertBaseType (isRange) (valType val)
    Val t <$> case val of
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 0]
        Val _ op  -> extractValue op [0]

mkRangeEnd :: InsCmp CompileState m => Value -> m Value
mkRangeEnd val = do
    Range t <- assertBaseType (isRange) (valType val)
    Val t <$> case val of
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 1]
        Val _ op  -> extractValue op [1]


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
        
        (I64, UnsafePtr) -> ptrtoint op LL.i64


valLoad :: InsCmp s m => Value -> m Value
valLoad (Val typ op)  = trace ("valLoad " ++ show typ) $ return (Val typ op)
valLoad (Ptr typ loc) = trace ("valLoad " ++ show typ) $ Val typ <$> load loc 0


storeBasic :: InsCmp CompileState m => Pointer -> Pointer -> m ()
storeBasic dst src = do 
    assert (typeof dst == typeof src) "storeBasic: type mismatch"
    store (loc dst) 0 =<< load (loc src) 0


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


newVal :: InsCmp CompileState m => Type -> m Pointer 
newVal typ = do 
    opType <- opTypeOf typ 
    p <- alloca opType Nothing 0 
    Val _ z <- mkZero typ
    store p 0 z
    return $ Pointer typ p


mkAlloca :: InsCmp CompileState m => Type -> m Value
mkAlloca typ = trace ("mkAlloca " ++ show typ) $ do
    opTyp <- opTypeOf typ
    Ptr typ <$> alloca opTyp Nothing 0
    

mkMalloc :: InsCmp CompileState m => Type -> Value -> m Value
mkMalloc typ len = trace ("mkMalloc " ++ show typ) $ do
    lenTyp <- assertBaseType isInt (valType len)
    lenOp <- valOp <$> valLoad len
    pi8 <- malloc =<< mul lenOp . valOp =<< sizeOf typ
    fmap (Ptr typ) $ bitcast pi8 . LL.ptr =<< opTypeOf typ



advancePointer :: InsCmp CompileState m => Pointer -> Value -> m Pointer
advancePointer (Pointer t p) idx = do
    I64 <- baseTypeOf (valType idx)
    op <- valOp <$> valLoad idx
    Pointer t <$> gep p [op]



valMemCpy :: InsCmp CompileState m => Value -> Value -> Value -> m ()
valMemCpy (Ptr dstTyp dst) (Ptr srcTyp src) len = trace "valMemCpy" $ do
    assert (dstTyp == srcTyp) "Types do not match"
    assertBaseType isInt (valType len)

    pDstI8 <- bitcast dst (LL.ptr LL.i8)
    pSrcI8 <- bitcast src (LL.ptr LL.i8)

    void $ memcpy pDstI8 pSrcI8 . valOp =<< mkIntInfix AST.Times len =<< sizeOf dstTyp


mkPrefix :: InsCmp CompileState m => AST.Operator -> Value -> m Value
mkPrefix operator val = do
    Val typ op <- valLoad val
    base <- baseTypeOf typ
    Val typ <$> case base of
        _ | isInt base -> case operator of
            AST.Plus -> return op
            AST.Minus -> mkInt typ 0 >>= \zero -> sub (valOp zero) op

        _ | isFloat base -> case operator of
            AST.Plus -> return op
            AST.Minus -> newFloat typ 0 >>= toVal >>= \zero -> fsub (valOp zero) op

        Bool -> case operator of
            AST.Not -> icmp P.EQ op (bit 0)

        Char -> case operator of
            AST.Minus -> sub (int8 0) op

        _ -> fail $ show base
        

mkIntInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkIntInfix operator a b = withErrorPrefix "int infix: " $ do
    assert (valType a == valType b) "type mismatch"
    assertBaseType (\t -> isInt t || t == Char) (valType a)
    Val typ opA <- valLoad a
    Val _   opB <- valLoad b
    case operator of
        AST.Plus   -> Val typ  <$> add opA opB
        AST.Minus  -> Val typ  <$> sub opA opB
        AST.Times  -> Val typ  <$> mul opA opB
        AST.Divide -> Val typ  <$> sdiv opA opB
        AST.Modulo -> Val typ  <$> srem opA opB
        AST.GT     -> Val Bool <$> icmp P.SGT opA opB
        AST.LT     -> Val Bool <$> icmp P.SLT opA opB
        AST.GTEq   -> Val Bool <$> icmp P.SGE opA opB
        AST.LTEq   -> Val Bool <$> icmp P.SLE opA opB
        AST.EqEq   -> Val Bool <$> icmp P.EQ opA opB
        AST.NotEq  -> Val Bool <$> icmp P.NE opA opB
        _        -> error ("int infix: " ++ show operator)
    
        
mkFloatInfix :: InsCmp CompileState m => AST.Operator -> Value -> Value -> m Value
mkFloatInfix operator a b = do
    assert (valType a == valType b) "Left side type does not match right side"
    let typ = valType a
    assertBaseType isFloat typ
    opA <- valOp <$> valLoad a
    opB <- valOp <$> valLoad b
    case operator of
        AST.Plus   -> Val typ <$> fadd opA opB
        AST.Minus  -> Val typ <$> fsub opA opB
        AST.Times  -> Val typ <$> fmul opA opB
        AST.Divide -> Val typ <$> fdiv opA opB
        AST.EqEq   -> Val Bool <$> fcmp P.OEQ opA opB
        AST.GT     -> Val Bool <$> fcmp P.OGT opA opB
        AST.LT     -> Val Bool <$> fcmp P.OLT opA opB
        _        -> error ("float infix: " ++ show operator)


boolInfix :: InsCmp CompileState m => AST.Operator -> Pointer -> Pointer -> m Pointer
boolInfix operator a b = do
    Bool <- baseTypeOf (typeof a)
    True <- return $ typeof a == typeof b

    ret <- newVal Bool
    opA <- load (loc a) 0
    opB <- load (loc b) 0
    op <- case operator of
        AST.OrOr   -> or opA opB
        AST.AndAnd -> and opA opB
        AST.EqEq   -> icmp P.EQ opA opB
        _        -> error ("bool infix: " ++ show operator)
    store (loc ret) 0 op
    return ret
