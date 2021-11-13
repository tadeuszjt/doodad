{-# LANGUAGE FlexibleContexts #-}
module Value where

import Prelude hiding (or, and)
import Data.Maybe
import Data.List hiding (or, and)
import Control.Monad
import Control.Monad.State hiding (void)
import Control.Monad.Trans

import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.Internal.EncodeAST
import LLVM.Internal.Coding hiding (alloca)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import qualified AST as S
import Monad
import CompileState
import Funcs
import Type


assertBaseType :: InsCmp CompileState m => (Type -> Bool) -> Type -> m Type
assertBaseType f typ = do
    base <- baseTypeOf typ
    assert (f base) ("invalid type of " ++ show typ)
    return base


valInt :: Integral i => Type -> i -> Value
valInt I8 n  = Val I8  $ int8  (fromIntegral n)
valInt I32 n = Val I32 $ int32 (fromIntegral n)
valInt I64 n = Val I64 $ int64 (fromIntegral n)


valI64 :: Integral i => i -> Value
valI64 = valInt I64 


valChar :: Char -> Value
valChar c = Val Char (int8 $ fromIntegral $ fromEnum c)


valBool :: Bool -> Value
valBool b = Val Bool (if b then bit 1 else bit 0)


valLoad :: InsCmp s m => Value -> m Value
valLoad (Val typ op)      = return (Val typ op)
valLoad (Ptr typ loc)     = Val typ <$> load loc 0
valLoad (Exp (S.Int _ n)) = return (valI64 n)


valStore :: InsCmp CompileState m => Value -> Value -> m ()
valStore (Ptr typ loc) val = do
    checkTypesMatch typ (valType val)
    case val of
        Ptr t l         -> store loc 0 =<< load l 0
        Val t o         -> store loc 0 o
        Exp (S.Int _ n) -> valStore (Ptr typ loc) =<< valLoad val


valSelect :: InsCmp CompileState m => Value -> Value -> Value -> m Value
valSelect cnd t f = do
    assertBaseType (==Bool) (valType cnd)
    checkTypesMatch (valType t) (valType f)
    return . Val (valType t) =<< select (valOp cnd) (valOp t) (valOp f)


valLocal :: InsCmp CompileState m => Type -> m Value
valLocal typ = do
    opTyp <- opTypeOf typ
    Ptr typ <$> alloca opTyp Nothing 0
    

valMalloc :: InsCmp CompileState m => Type -> Value -> m Value
valMalloc typ len = do
    pi8  <- malloc . valOp =<< valsInfix S.Times len =<< valI64 <$> sizeOf typ
    Ptr typ <$> (bitcast pi8 . LL.ptr =<< opTypeOf typ)


valsInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valsInfix operator (Exp (S.Int p a)) (Exp (S.Int _ b)) = return $ case operator of
    S.Plus   -> Exp (S.Int p (a + b))
    S.Minus  -> Exp (S.Int p (a - b))
    S.Times  -> Exp (S.Int p (a * b))
    S.Divide -> Exp (S.Int p (a `div` b))
    S.GT     -> valBool (a > b)
    S.LT     -> valBool (a < b)
valsInfix operator (Exp (S.Int _ a)) b = do
    typ <- assertBaseType isInt (valType b)
    valsInfix operator (valInt typ a) b
valsInfix operator a (Exp (S.Int _ b)) = do
    typ <- assertBaseType isInt (valType a)
    valsInfix operator a (valInt typ b)
valsInfix operator a b = do
    baseA <- baseTypeOf (valType a)
    baseB <- baseTypeOf (valType b)
    checkTypesMatch baseA baseB

    Val _ opA <- valLoad a
    Val _ opB <- valLoad b
    valsInfix' operator (valType a) baseA opA opB
    where
        valsInfix' :: InsCmp CompileState m => S.Op -> Type -> Type -> LL.Operand -> LL.Operand -> m Value
        valsInfix' operator typ base opA opB
            | isInt base || base == Char = case operator of
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
                _        -> error ("int infix: " ++ show operator)
            | typ == Bool = case operator of
                S.OrOr   -> Val Bool <$> or opA opB
                S.AndAnd -> Val Bool <$> and opA opB
                _        -> error ("bool infix: " ++ show operator)
            | otherwise  = err ("cannot use op: " ++ show operator ++ " for: " ++ show typ)
        

valNot :: InsCmp CompileState m => Value -> m Value
valNot val = do
    assertBaseType (== Bool) (valType val)
    Val (valType val) <$> (icmp P.EQ (bit 0) . valOp =<< valLoad val)


valPtrIdx :: InsCmp s m => Value -> Value -> m Value
valPtrIdx (Ptr typ loc) idx = do
    Val I64 i <- valLoad idx
    Ptr typ <$> gep loc [i]


valTupleSet :: InsCmp CompileState m => Value -> Int -> Value -> m Value
valTupleSet tup i val = do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (fromIntegral i < length ts) "invalid tuple index"

    case tup of
        Ptr _ _ -> do
            ptr <- valTupleIdx tup i
            valStore ptr val >> return tup

        Val _ _ -> do
            op <- valOp <$> valLoad val
            Val (valType tup) <$> insertValue (valOp tup) op [fromIntegral i]


valTupleIdx :: InsCmp CompileState m => Value -> Int -> m Value
valTupleIdx tup i = do
    Tuple ts <- assertBaseType isTuple (valType tup)
    case tup of
        Ptr _ loc          -> Ptr (ts !! i) <$> gep loc [int32 0, int32 $ fromIntegral i]
        Val _ op           -> Val (ts !! i) <$> extractValue op [fromIntegral i]
        Exp (S.Tuple _ es) -> return $ Exp (es !! i)


valArrayIdx :: InsCmp CompileState m => Value -> Value -> m Value
valArrayIdx (Ptr (Array n t) loc) idx = do
    Val idxTyp idx <- valLoad idx
    assert (isInt idxTyp) "array index isn't an integer"
    Ptr t <$> gep loc [int64 0, idx]


valArrayConstIdx :: InsCmp CompileState m => Value -> Int -> m Value
valArrayConstIdx val i = do
    Array n t <- assertBaseType isArray (valType val)
    case val of
        Ptr _ loc -> Ptr t <$> gep loc [int64 0, int64 (fromIntegral i)]
        Val _ op  -> Val t <$> extractValue op [fromIntegral i]


valMemCpy :: InsCmp CompileState m => Value -> Value -> Value -> m ()
valMemCpy (Ptr dstTyp dst) (Ptr srcTyp src) len = do
    checkTypesMatch dstTyp srcTyp
    pDstI8 <- bitcast dst (LL.ptr LL.i8)
    pSrcI8 <- bitcast src (LL.ptr LL.i8)
    void $ memcpy pDstI8 pSrcI8 . valOp =<< valsInfix S.Times len . valI64 =<< sizeOf dstTyp



valIsExpr :: Value -> Bool
valIsExpr (Exp _) = True
valIsExpr _       = False


checkTypesMatch :: BoM CompileState m => Type -> Type -> m ()
checkTypesMatch typA typB
    | isSimple typA  = assert (typA == typB) str
    | isTable typA   = assert (typA == typB) str
    | isTuple typA   = assert (typA == typB) str
    | isTypedef typA = assert (typA == typB) str
    | isADT typA     = assert (typA == typB) str
    | otherwise      = err (show typA ++ " does not match " ++ show typB)
    where
        str = show typA ++ " does not match " ++ show typB


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef s -> do ObType t _ <- look s KeyType; baseTypeOf t
    Named s t -> baseTypeOf t
    _         -> return typ


pureTypeOf :: ModCmp CompileState m => Type -> m Type
pureTypeOf typ = case typ of
    Void             -> return Void
    Typedef s        -> do ObType t _ <- look s KeyType; pureTypeOf t
    Table ts         -> Table   <$> mapM pureTypeOf ts
    Tuple ts         -> Tuple   <$> mapM pureTypeOf ts
    Array n t        -> Array n <$> pureTypeOf t
    ADT xs           -> do --ADT     <$> mapM pureTypeOf ts
        xs' <- forM xs $ \(s, t) -> do
            t' <- pureTypeOf t
            return (s, t')
        return (ADT xs')
    Named n t        -> pureTypeOf t
    _ | isSimple typ -> return typ
    _                -> error (show typ)


zeroOf :: ModCmp CompileState m => Type -> m Value
zeroOf typ = case typ of
    _ | isInt typ -> return (valInt typ 0)
    Bool          -> return (valBool False)
    Char          -> return (valChar '\0')
    Typedef sym   -> Val typ . valOp <$> (zeroOf =<< baseTypeOf typ)
    Array n t     -> Val typ . array . replicate n . toCons . valOp <$> zeroOf t
    ADT [(s, t)]  -> Val typ . cons . C.Null . LL.ptr <$> opTypeOf t -- ADT with one type, has no enum
    Tuple ts      -> Val typ . struct Nothing False . map (toCons . valOp) <$> mapM zeroOf ts

    Table ts      -> do
        let zi64 = toCons (int64 0)
        zptrs <- mapM (return . C.IntToPtr zi64 . LL.ptr <=< opTypeOf) ts
        return $ Val typ $ struct Nothing False (zi64:zi64:zptrs)

    _ -> err ("no zero val for: " ++ show typ)


opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = case typ of
    Void      -> return LL.VoidType
    I16       -> return LL.i16
    I32       -> return LL.i32
    I64       -> return LL.i64
    Char      -> return LL.i8
    Bool      -> return LL.i1
    Tuple ts  -> LL.StructureType False <$> mapM opTypeOf ts
    Array n t -> LL.ArrayType (fromIntegral n) <$> opTypeOf t
    Named n t -> opTypeOf t
    ADT []    -> error ""
    ADT [t]   -> return (LL.ptr LL.i8)
    ADT ts    -> return $ LL.StructureType False [LL.i64, LL.ptr LL.i8]
    Table ts  -> LL.StructureType False . ([LL.i64, LL.i64] ++) . map LL.ptr <$> mapM opTypeOf ts

    Typedef s -> do
        ObType t namem <- look s KeyType
        maybe (opTypeOf t) (return . LL.NamedTypeReference) namem

    _ -> error (show typ) 


sizeOf :: InsCmp CompileState m => Type -> m Int
sizeOf typ = size =<< opTypeOf =<< pureTypeOf typ
    where
        size :: InsCmp CompileState m => LL.Type -> m Int
        size typ = do
            ctx <- gets context
            dl <- gets dataLayout
            liftIO $ return . fromIntegral =<< FFI.getTypeAllocSize dl =<< runEncodeAST ctx (encodeM typ)

