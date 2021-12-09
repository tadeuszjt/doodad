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


valI64 :: Integral i => i -> Value
valI64 n = Val I64 $ int64 (fromIntegral n)


valChar :: Char -> Value
valChar c = Val Char (int8 $ fromIntegral $ fromEnum c)


valBool :: Bool -> Value
valBool b = Val Bool (if b then bit 1 else bit 0)


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


valZero :: InsCmp CompileState m => Type -> m Value
valZero typ = trace ("valZero " ++ show  typ) $ do
    case typ of
        Typedef sym -> do
            ObType t namem <- look sym KeyType
            fmap (Val typ . valOp) $ valZero' namem =<< baseTypeOf t
        _ -> valZero' Nothing typ

    where
        valZero' :: InsCmp CompileState m => Maybe Name -> Type -> m Value
        valZero' namem typ =
            case typ of
                _ | isInt typ   -> valInt typ 0
                _ | isFloat typ -> valFloat typ 0.0
                Bool            -> return (valBool False)
                Char            -> return (valChar '\0')
                Typedef sym     -> fmap (Val typ . valOp) $ valZero =<< baseTypeOf typ
                Array n t       -> Val typ . array . replicate n . toCons . valOp <$> valZero t
                ADT _
                    | isEmptyADT typ  -> return $ Val typ $ cons $ C.Null (LL.ptr LL.i8)
                    | isEnumADT typ   -> return $ Val typ (int64 0)
                    | isNormalADT typ -> err "Cannot zero-construct ADT type. Use field constructor."
                Tuple xs        -> Val typ . struct namem False . map (toCons . valOp) <$> mapM (valZero . snd) xs
                Table ts        -> do
                    let zi64 = toCons (int64 0)
                    zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
                    return $ Val typ $ struct namem False (zi64:zi64:zptrs)
                _ -> error ("valZero: " ++  show typ)


valLoad :: InsCmp s m => Value -> m Value
valLoad (Val typ op)  = trace ("valLoad " ++ show typ) $ return (Val typ op)
valLoad (Ptr typ loc) = trace ("valLoad " ++ show typ) $ Val typ <$> load loc 0


valStore :: InsCmp CompileState m => Value -> Value -> m ()
valStore (Ptr typ loc) val = trace "valStore" $ do
    checkTypesCompatible typ (valType val)
    case val of
        Ptr t l -> store loc 0 =<< load l 0
        Val t o -> store loc 0 o
        Exp _   -> err "Cannot store"


valSelect :: InsCmp CompileState m => Value -> Value -> Value -> m Value
valSelect cnd t f = trace "valSelect" $ do
    assertBaseType (==Bool) (valType cnd)
    assert (valType t == valType f) "Types do not match"
    Val (valType t) <$> select (valOp cnd) (valOp t) (valOp f)


valLocal :: InsCmp CompileState m => Type -> m Value
valLocal typ = trace ("valLocal " ++ show typ) $ do
    opTyp <- opTypeOf typ
    Ptr typ <$> alloca opTyp Nothing 0
    

valArrayIdx :: InsCmp CompileState m => Value -> Value -> m Value
valArrayIdx (Ptr typ loc) idx = trace "valArrayIdx" $ do
    Array n t <- assertBaseType isArray typ
    Val idxTyp idx <- valLoad idx
    assert (isInt idxTyp) "array index isn't an integer"
    Ptr t <$> gep loc [int64 0, idx]


valArrayConstIdx :: InsCmp CompileState m => Value -> Int -> m Value
valArrayConstIdx val i = trace "valArrayConstIdx" $ do
    Array n t <- assertBaseType isArray (valType val)
    case val of
        Ptr _ loc -> Ptr t <$> gep loc [int64 0, int64 (fromIntegral i)]
        Val _ op  -> Val t <$> extractValue op [fromIntegral i]


valMalloc :: InsCmp CompileState m => Type -> Value -> m Value
valMalloc typ len = trace ("valMalloc " ++ show typ) $ do
    lenTyp <- assertBaseType isInt (valType len)
    lenOp <- valOp <$> valLoad len
    pi8 <- malloc =<< mul lenOp . valOp =<< sizeOf typ
    fmap (Ptr typ) $ bitcast pi8 . LL.ptr =<< opTypeOf typ


valPtrIdx :: InsCmp s m => Value -> Value -> m Value
valPtrIdx (Ptr typ loc) idx = trace ("valPtrIdx " ++ show typ) $ do
    Val I64 i <- valLoad idx
    Ptr typ <$> gep loc [i]


valMemCpy :: InsCmp CompileState m => Value -> Value -> Value -> m ()
valMemCpy (Ptr dstTyp dst) (Ptr srcTyp src) len = trace "valMemCpy" $ do
    assert (dstTyp == srcTyp) "Types do not match"
    assertBaseType isInt (valType len)

    pDstI8 <- bitcast dst (LL.ptr LL.i8)
    pSrcI8 <- bitcast src (LL.ptr LL.i8)

    void $ memcpy pDstI8 pSrcI8 . valOp =<< valsInfix S.Times len =<< sizeOf dstTyp


valNot :: InsCmp CompileState m => Value -> m Value
valNot val = trace "valNot" $ do
    assertBaseType (== Bool) (valType val)
    Val (valType val) <$> (icmp P.EQ (bit 0) . valOp =<< valLoad val)

        
valsInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
valsInfix operator a b = trace ("valsInfix " ++ show operator) $ case (a, b) of
    (Exp ea, Exp eb) -> do
        return $ Exp (exprInfix operator ea eb)

    (Exp (S.Int _ i), _) -> do
        assertBaseType isInt (valType b)
        val <- valInt (valType b) i
        valsInfix operator val b

    (_, Exp (S.Int _ i)) -> do
        assertBaseType isInt (valType a)
        val <- valInt (valType a) i
        valsInfix operator a val

    (_, Exp _) -> err ""

    (Exp _, _) -> err ""

    _ -> do
        Val _ opA <- valLoad a
        Val _ opB <- valLoad b

        checkTypesCompatible (valType a) (valType b)
        base <- baseTypeOf (valType a)

        case base of
            Bool             -> boolInfix (valType a) operator opA opB
            Char             -> intInfix (valType a) operator opA opB
            _ | isInt base   -> intInfix (valType a) operator opA opB
            _ | isFloat base -> floatInfix (valType a) operator opA opB
            _                -> err ("Operator " ++ show operator ++ " undefined for types")

    where 
        exprInfix operator exprA exprB = case (operator, exprA, exprB) of
            (S.Plus, S.Int p a, S.Int _ b) -> S.Int p (a + b)

        boolInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        boolInfix typ operator opA opB = case operator of
            S.OrOr   -> Val typ <$> or opA opB
            S.AndAnd -> Val typ <$> and opA opB
            S.EqEq   -> Val typ <$> icmp P.EQ opA opB
            _        -> error ("bool infix: " ++ show operator)
        
        intInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        intInfix typ operator opA opB = case operator of
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

        floatInfix :: InsCmp CompileState m => Type -> S.Op -> LL.Operand -> LL.Operand -> m Value
        floatInfix typ operator opA opB = case operator of
            S.Plus   -> Val typ <$> fadd opA opB
            S.Minus  -> Val typ <$> fsub opA opB
            S.Times  -> Val typ <$> fmul opA opB
            S.Divide -> Val typ <$> fdiv opA opB
            S.EqEq   -> Val Bool <$> fcmp P.OEQ opA opB
            _        -> error ("float infix: " ++ show operator)
        
