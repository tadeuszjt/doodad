{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Typeof where

import Control.Monad.Trans
import Control.Monad.State hiding (void)
import GHC.Float
import Debug.Trace

import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.Constant as C
import qualified LLVM.Internal.FFI.DataLayout as FFI
import LLVM.AST.Name
import LLVM.Internal.EncodeAST
import LLVM.Internal.Coding hiding (alloca)
import LLVM.IRBuilder.Constant

import Type
import Monad
import State
import Funcs

assertBaseType :: InsCmp CompileState m => (Type -> Bool) -> Type -> m Type
assertBaseType f typ = do
    base <- baseTypeOf typ
    assert (f base) ("invalid type of " ++ show typ)
    return base


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

    Func ts rt -> do
        rt' <- opTypeOf rt
        ts' <- mapM opTypeOf ts
        return $ LL.ptr (LL.FunctionType rt' ts' False)
    _         -> error (show typ) 



zeroOf :: InsCmp CompileState m => Type -> m Value
zeroOf typ = trace ("zeroOf " ++ show  typ) $ do
    case typ of
        Typedef sym -> do
            ObType t namem <- look sym KeyType
            fmap (Val typ . valOp) $ zeroOf' namem =<< baseTypeOf t
        _ -> zeroOf' Nothing typ

    where
        zeroOf' :: InsCmp CompileState m => Maybe Name -> Type -> m Value
        zeroOf' namem typ =
            case typ of
                _ | isInt typ          -> valInt typ 0
                _ | isFloat typ        -> valFloat typ 0.0
                Bool                   -> return (valBool False)
                Char                   -> return (valChar '\0')
                Typedef sym            -> fmap (Val typ . valOp) $ zeroOf =<< baseTypeOf typ
                Array n t              -> Val typ . array . replicate n . toCons . valOp <$> zeroOf t
                ADT _
                    | isEmptyADT typ -> return $ Val typ $ cons $ C.Null (LL.ptr LL.i8)
                    | isEnumADT typ  -> return $ Val typ (int64 0)
                Tuple xs               -> Val typ . struct namem False . map (toCons . valOp) <$> mapM (zeroOf . snd) xs
                Table ts -> do
                    let zi64 = toCons (int64 0)
                    zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
                    return $ Val typ $ struct namem False (zi64:zi64:zptrs)


pureTypeOf :: ModCmp CompileState m => Type -> m Type
pureTypeOf initialType = case initialType of
    Typedef s      -> do ObType t _ <- look s KeyType; pureTypeOf' t
    Void           -> return Void
    Tuple xs       -> fmap Tuple $ forM xs $ \(_, t) -> ("",) <$> pureTypeOf' t
    Table ts       -> Table <$> mapM pureTypeOf' ts
    ADT xs         -> fmap ADT $ forM xs $ \(s, t) -> (s,) <$> pureTypeOf' t
    t | isSimple t -> return t
    Func ts rt     -> do
        rt' <- pureTypeOf' rt
        ts' <- mapM pureTypeOf' ts
        return (Func ts' rt')

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

            Tuple xs -> fmap Tuple $ forM xs $ \(s, t) ->
                if t == initialType
                then err (show initialType ++ " is self-referential")
                else (s,) <$> pureTypeOf' t


            Typedef s -> do ObType t _ <- look s KeyType; pureTypeOf' t

            x -> error ("pureTypeOf': " ++ show x)


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

