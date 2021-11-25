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


opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = case typ of
    Void      -> return LL.VoidType
    I16       -> return LL.i16
    I32       -> return LL.i32
    I64       -> return LL.i64
    F64       -> return LL.double
    Char      -> return LL.i8
    Bool      -> return LL.i1
    Tuple xs  -> LL.StructureType False <$> mapM opTypeOf (map snd xs)
    Array n t -> LL.ArrayType (fromIntegral n) <$> opTypeOf t
    ADT xs
        | isEmptyADT typ  -> return (LL.ptr LL.i8)
        | isPtrADT typ    -> return (LL.ptr LL.i8)
        | isEnumADT typ   -> return LL.i64
        | isNormalADT typ -> return $ LL.StructureType False [LL.i64, LL.ptr LL.i8]
    Table ts  -> do
        ps <- map LL.ptr <$> mapM opTypeOf ts
        return $ LL.StructureType False (LL.i64:LL.i64:ps)
    Typedef s -> do
        ObType t namem <- look s KeyType
        maybe (opTypeOf t) (return . LL.NamedTypeReference) namem
    _         -> error (show typ) 



zeroOf :: InsCmp CompileState m => Type -> m Value
zeroOf typ = case typ of
    _ | isInt typ          -> valInt typ 0
    _ | isFloat typ        -> valFloat typ 0.0
    Bool                   -> return (valBool False)
    Char                   -> return (valChar '\0')
    Typedef sym            -> fmap (Val typ . valOp) $ zeroOf =<< baseTypeOf typ
    Array n t              -> Val typ . array . replicate n . toCons . valOp <$> zeroOf t
    ADT _
        | isEmptyADT typ -> return $ Val typ $ cons $ C.Null (LL.ptr LL.i8)
        | isEnumADT typ  -> return $ Val typ (int64 0)
    Tuple xs               -> Val typ . struct Nothing False . map (toCons . valOp) <$> mapM (zeroOf . snd) xs
    Table ts -> do
        let zi64 = toCons (int64 0)
        zptrs <- mapM (return . C.IntToPtr zi64 . LL.ptr <=< opTypeOf) ts
        return $ Val typ $ struct Nothing False (zi64:zi64:zptrs)

    _ -> err ("no zero val for: " ++ show typ)


pureTypeOf :: ModCmp CompileState m => Type -> m Type
pureTypeOf initialType = case initialType of
    Typedef s      -> do ObType t _ <- look s KeyType; pureTypeOf' t
    Void           -> return Void
    Tuple xs       -> fmap Tuple $ forM xs $ \(_, t) -> ("",) <$> pureTypeOf' t
    Table ts       -> Table <$> mapM pureTypeOf' ts
    ADT xs         -> fmap ADT $ forM xs $ \(s, t) -> ("",) <$> pureTypeOf' t
    t | isSimple t -> return t
    x              -> error ("pureTypeOf: " ++ show x)

    where
        pureTypeOf' :: ModCmp CompileState m => Type -> m Type
        pureTypeOf' typ = case typ of
            Void           -> return Void
            t | isSimple t -> return t
            ADT xs -> fmap ADT $ forM xs $ \(s, t) ->
                if t == initialType
                then return (s, Self)
                else (s,) <$> pureTypeOf' t

            Table ts -> fmap Table $ forM ts $ \t ->
                if t == initialType
                then err (show initialType ++ " is self-referential")
                else pureTypeOf' t

            Typedef s -> do ObType t _ <- look s KeyType; pureTypeOf' t

            x -> error ("pureTypeOf': " ++ show x)


assertBaseType :: InsCmp CompileState m => (Type -> Bool) -> Type -> m Type
assertBaseType f typ = do
    base <- baseTypeOf typ
    assert (f base) ("invalid type of " ++ show typ)
    return base


valResolveContextual :: InsCmp CompileState m => Value -> m Value
valResolveContextual val = case val of
    Exp (S.Int p n)   -> valInt I64 n
    Exp (S.Float p f) -> valFloat F64 f
    Exp (S.Null p)    -> zeroOf $ ADT [("", Void)]
    Ptr _ _           -> return val
    Val _ _           -> return val
    _                 -> error ("can't resolve contextual: " ++ show val)


valInt :: InsCmp CompileState m => Integral i => Type -> i -> m Value
valInt typ n = do
    base <- assertBaseType isInt typ
    return $ case base of
        I8  -> Val typ $ int8 (fromIntegral n)
        I32 -> Val typ $ int32 (fromIntegral n)
        I64 -> Val typ $ int64 (fromIntegral n)


valFloat :: InsCmp CompileState m => Type -> Double -> m Value
valFloat typ f = do
    base <- assertBaseType isFloat typ
    return $ case base of
        F32 -> Val typ $ single (double2Float f)
        F64 -> Val typ $ double f


valI64 :: Integral i => i -> Value
valI64 n = Val I64 $ int64 (fromIntegral n)


valChar :: Char -> Value
valChar c = Val Char (int8 $ fromIntegral $ fromEnum c)


valBool :: Bool -> Value
valBool b = Val Bool (if b then bit 1 else bit 0)


valLoad :: InsCmp s m => Value -> m Value
valLoad (Val typ op)  = return (Val typ op)
valLoad (Ptr typ loc) = Val typ <$> load loc 0


valStore :: InsCmp CompileState m => Value -> Value -> m ()
valStore (Ptr typ loc) val = do
    assert (not $ valIsContextual val) "contextual 73"
    case val of
        Ptr t l -> store loc 0 =<< load l 0
        Val t o -> store loc 0 o


valSelect :: InsCmp CompileState m => Value -> Value -> Value -> m Value
valSelect cnd t f = do
    assert (not $ valIsContextual t) "contextual 84"
    assert (not $ valIsContextual f) "contextual 84"
    assertBaseType (==Bool) (valType cnd)
    checkTypesMatch (valType t) (valType f)
    return . Val (valType t) =<< select (valOp cnd) (valOp t) (valOp f)


valLocal :: InsCmp CompileState m => Type -> m Value
valLocal typ = do
    opTyp <- opTypeOf typ
    Ptr typ <$> alloca opTyp Nothing 0
    

valMalloc :: InsCmp CompileState m => Type -> Value -> m Value
valMalloc typ len = do
    lenTyp <- assertBaseType isInt (valType len)
    siz <- sizeOf =<< baseTypeOf typ

    pi8 <- malloc =<< mul (valOp len) (int64 $ fromIntegral siz)
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
    i <- valInt typ a  
    valsInfix operator i b
valsInfix operator a (Exp (S.Int _ b)) = do
    typ <- assertBaseType isInt (valType a)
    valsInfix operator a =<< valInt typ b
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
            | isFloat base = case operator of
                S.Plus   -> Val typ <$> fadd opA opB
                S.Minus  -> Val typ <$> fsub opA opB
                S.Times  -> Val typ <$> fmul opA opB
                S.Divide -> Val typ <$> fdiv opA opB
                S.EqEq   -> Val Bool <$> fcmp P.OEQ opA opB
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


valArrayIdx :: InsCmp CompileState m => Value -> Value -> m Value
valArrayIdx (Ptr (Array n t) loc) idx = do
    Val idxTyp idx <- valLoad idx
    assert (isInt idxTyp) "array index isn't an integer"
    Ptr t <$> gep loc [int64 0, idx]


valArrayConstIdx :: InsCmp CompileState m => Value -> Int -> m Value
valArrayConstIdx val i = do
    assert (not $ valIsContextual val) "contextual 172"
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


checkTypesMatch :: BoM CompileState m => Type -> Type -> m ()
checkTypesMatch typA typB
    | isSimple typA  = assert (typA == typB) str
    | isTable typA   = assert (typA == typB) str
    | isTuple typA   = assert (typA == typB) str
    | isTypedef typA = assert (typA == typB) str
    | isADT typA     = assert (typA == typB) str
    | otherwise      = err str
    where
        str = show typA ++ " does not match " ++ show typB


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef s -> do ObType t _ <- look s KeyType; baseTypeOf t
    _         -> return typ


sizeOf :: InsCmp CompileState m => Type -> m Int
sizeOf typ       = size =<< opTypeOf =<< pureTypeOf typ
size :: InsCmp CompileState m => LL.Type -> m Int
size typ = do
    ctx <- gets context
    dl <- gets dataLayout
    liftIO $ return . fromIntegral =<< FFI.getTypeAllocSize dl =<< runEncodeAST ctx (encodeM typ)

