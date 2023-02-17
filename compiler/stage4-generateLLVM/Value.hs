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


toValue (Val t p) = Value2 t p

fromValue (Value2 t p) = Val t p

toPointer (Ptr t p) = Pointer t p

fromPointer (Pointer t p) = Ptr t p


toVal :: InsCmp CompileState m => Pointer -> m Value 
toVal ptr = 
    fromValue <$> pload ptr


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


mkEnum :: InsCmp CompileState m => Integral i => Type -> i -> m Value2
mkEnum typ n = do
    assertBaseType (== Enum) typ
    return $ Value2 typ $ int64 (fromIntegral n)


newRange :: InsCmp CompileState m => Value2 -> Value2 -> m Pointer
newRange start end = do
    assert (typeof start == typeof end) $ "range types do not match"
    isDataType <- isDataType (typeof start)
    assert (not isDataType) "simple types so valLoad is mk"

    range    <- newVal $ Range (typeof start) -- LEAVE THIS ONE FOR NOW
    startDst <- rangeStart range
    endDst   <- rangeEnd range

    storeBasicVal startDst start
    storeBasicVal endDst end
    return range



-- TODO, is this the cause of the struct packed/non-packed bug?
mkZero :: InsCmp CompileState m => Type -> m Value
mkZero typ = trace ("mkZero " ++ show  typ) $ do
    namem <- Map.lookup typ <$> gets typeNameMap
    base <- baseTypeOf typ
    case base of
        I64                -> return $ Val typ (int64 0)
        I32                -> return $ Val typ (int32 0)
        F32                -> return $ Val typ (single 0.0)
        F64                -> return $ Val typ (double 0.0)
        Enum               -> return $ Val typ (int64 0)
        Bool               -> return $ Val typ (bit 0)
        Char               -> return $ Val typ (int8 0)
        Range t            -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero [t, t]
        Array n t          -> Val typ . array . replicate n . toCons . valOp <$> mkZero t
        Tuple ts           -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero ts
        Table ts           -> Val typ . struct namem False . ([zi64, zi64] ++) <$> map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
        Sparse ts          -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero [Table ts, Table [I64]]
        Map tk tv          -> Val typ . struct namem False . map (toCons . valOp) <$> mapM mkZero [Table [tk], Sparse [tv]]
        --UnsafePtr          -> return $ Val typ $ cons $ C.IntToPtr zi64 (LL.ptr LL.VoidType)
        _ -> error ("mkZero: " ++  show typ)
        where
            zi64 = toCons (int64 0)


rangeStart :: InsCmp CompileState m => Pointer -> m Pointer
rangeStart range = do
    Range t <- baseTypeOf (typeof range)
    Pointer t <$> gep (loc range) [int32 0, int32 0]


rangeEnd :: InsCmp CompileState m => Pointer -> m Pointer
rangeEnd range = do
    Range t <- baseTypeOf (typeof range)
    Pointer t <$> gep (loc range) [int32 0, int32 1]



pload :: InsCmp CompileState m => Pointer -> m Value2
pload ptr = do
    assertBaseType isSimple (typeof ptr)
    Value2 (typeof ptr) <$> load (loc ptr) 0


convertNumber :: InsCmp CompileState m => Type -> Value2 -> m Value2
convertNumber typ val = do
    op <- return (op val)
    base <- baseTypeOf typ
    baseVal <- baseTypeOf (typeof val)
    fmap (Value2 typ) $ case (base, baseVal) of
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

storeBasicVal :: InsCmp CompileState m => Pointer -> Value2 -> m ()
storeBasicVal dst src = do 
    assert (typeof dst == typeof src) "storeBasic: type mismatch"
    store (loc dst) 0 (op src) 

valStore :: InsCmp CompileState m => Value -> Value -> m ()
valStore (Ptr typ loc) val = trace "valStore" $ do
    base <- baseTypeOf typ
    valBase <- baseTypeOf (typeof val)
    assert (base == valBase) "types incompatible"
    case val of
        Ptr t l -> store loc 0 =<< load l 0
        Val t o -> store loc 0 o


newVal :: InsCmp CompileState m => Type -> m Pointer 
newVal typ = do 
    opType <- opTypeOf typ 
    p <- alloca opType Nothing 0 
    base <- baseTypeOf typ
    case base of 
        Tuple ts -> forM_ (zip ts [0..]) $ \(t, i) -> do 
            pelm <- gep p [int32 0, int32 $ fromIntegral i]
            storeBasic (Pointer t pelm) =<< newVal t
        _ -> do
            Val _ z <- mkZero typ
            store p 0 z
    return $ Pointer typ p


pMalloc :: InsCmp CompileState m => Type -> Value2 -> m Pointer
pMalloc typ len = trace ("mkMalloc " ++ show typ) $ do
    lenTyp <- assertBaseType isInt (typeof len)
    pi8 <- malloc =<< mul (op len) . valOp =<< sizeOf typ
    fmap (Pointer typ) $ bitcast pi8 . LL.ptr =<< opTypeOf typ


advancePointer :: InsCmp CompileState m => Pointer -> Value2 -> m Pointer
advancePointer ptr idx = do
    I64 <- baseTypeOf (typeof idx)
    Pointer (typeof ptr) <$> gep (loc ptr) [op idx]


memCpy :: InsCmp CompileState m => Pointer -> Pointer -> Value2 -> m ()
memCpy (Pointer dstTyp dst) (Pointer srcTyp src) len = trace "valMemCpy" $ do
    True <- return (dstTyp == srcTyp)
    I64 <- baseTypeOf (typeof len)

    Val I64 siz <- sizeOf dstTyp
    pDstI8 <- bitcast dst (LL.ptr LL.i8)
    pSrcI8 <- bitcast src (LL.ptr LL.i8)
    void $ memcpy pDstI8 pSrcI8 =<< mul siz (op len)


prefix :: InsCmp CompileState m => AST.Operator -> Value2 -> m Value2
prefix operator val = do
    base <- baseTypeOf (typeof val)
    Value2 (typeof val) <$> case base of
        _ | isInt base -> case operator of
            AST.Plus -> return (op val)
            AST.Minus -> mkZero (typeof val) >>= \zero -> op <$> (intInfix AST.Minus (toValue zero) val)

        _ | isFloat base -> case operator of
            AST.Plus -> return (op val)
            AST.Minus -> newFloat (typeof val) 0 >>= toVal >>= \zero -> fsub (valOp zero) (op val)

        Bool -> case operator of
            AST.Not -> icmp P.EQ (op val) (bit 0)

        Char -> case operator of
            AST.Minus -> sub (int8 0) (op val)

        _ -> fail $ show base
        

intInfix :: InsCmp CompileState m => AST.Operator -> Value2 -> Value2 -> m Value2
intInfix operator a b = withErrorPrefix "int infix: " $ do
    True <- return (typeof a == typeof b)
    assertBaseType (\t -> isInt t || t == Char) (typeof a)
    case operator of
        AST.Plus   -> Value2 (typeof a) <$> add (op a) (op b)
        AST.Minus  -> Value2 (typeof a) <$> sub (op a) (op b)
        AST.Times  -> Value2 (typeof a) <$> mul (op a) (op b)
        AST.Divide -> Value2 (typeof a) <$> sdiv (op a) (op b)
        AST.Modulo -> Value2 (typeof a) <$> srem (op a) (op b)
        AST.GT     -> Value2 Bool <$> icmp P.SGT (op a) (op b)
        AST.LT     -> Value2 Bool <$> icmp P.SLT (op a) (op b)
        AST.GTEq   -> Value2 Bool <$> icmp P.SGE (op a) (op b)
        AST.LTEq   -> Value2 Bool <$> icmp P.SLE (op a) (op b)
        AST.EqEq   -> Value2 Bool <$> icmp P.EQ (op a) (op b)
        AST.NotEq  -> Value2 Bool <$> icmp P.NE (op a) (op b)
        _        -> error ("int infix: " ++ show operator)
    
        
floatInfix :: InsCmp CompileState m => AST.Operator -> Value2 -> Value2 -> m Value2
floatInfix operator a b = do
    assert (typeof a == typeof b) "Left side type does not match right side"
    let typ = typeof a
    assertBaseType isFloat typ
    case operator of
        AST.Plus   -> Value2 typ <$> fadd (op a) (op b)
        AST.Minus  -> Value2 typ <$> fsub (op a) (op b)
        AST.Times  -> Value2 typ <$> fmul (op a) (op b)
        AST.Divide -> Value2 typ <$> fdiv (op a) (op b)
        AST.EqEq   -> Value2 Bool <$> fcmp P.OEQ (op a) (op b)
        AST.GT     -> Value2 Bool <$> fcmp P.OGT (op a) (op b)
        AST.LT     -> Value2 Bool <$> fcmp P.OLT (op a) (op b)
        _        -> error ("float infix: " ++ show operator)


boolInfix :: InsCmp CompileState m => AST.Operator -> Value2 -> Value2 -> m Value2
boolInfix operator a b = do
    Bool <- baseTypeOf (typeof a)
    True <- return $ typeof a == typeof b
    op <- case operator of
        AST.OrOr   -> or (op a) (op b)
        AST.AndAnd -> and (op a) (op b)
        AST.EqEq   -> icmp P.EQ (op a) (op b)
        _        -> error ("bool infix: " ++ show operator)
    return $ Value2 Bool op
