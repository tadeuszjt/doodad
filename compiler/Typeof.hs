{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Typeof where

import Control.Monad.Trans
import Control.Monad.State hiding (void)
import GHC.Float

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
import Trace
import Error

assertBaseType :: InsCmp CompileState m => (Type -> Bool) -> Type -> m Type
assertBaseType f typ = trace "assertBaseType" $ do
    base <- baseTypeOf typ
    assert (f base) ("Invalid base type of: " ++ show typ)
    return base


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef sym -> trace ("baseTypeOf " ++ show sym) $ do ObType t _ <- look sym KeyType; baseTypeOf t
    _           -> return typ


sizeOf :: InsCmp CompileState m => Type -> m Value
sizeOf typ = trace "sizeOf" $ do
    opType <- opTypeOf typ
    return $ Val I64 $ cons $ C.SExt (C.sizeof opType) (LL.IntegerType 64)



isDataType :: InsCmp CompileState m => Type -> m Bool
isDataType typ = do
    base <- baseTypeOf typ
    case base of
        _ | isSimple base -> return False
        Void              -> return False
        String            -> return False
        Tuple ts          -> any (== True) <$> mapM isDataType ts
        Array n t         -> isDataType t
        _                 -> return True



opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = trace ("opTypOf " ++ show typ) $ case typ of
    Void      -> return LL.VoidType
    I8        -> return LL.i8
    I16       -> return LL.i16
    I32       -> return LL.i32
    I64       -> return LL.i64
    F32       -> return LL.float
    F64       -> return LL.double
    Char      -> return LL.i8
    Bool      -> return LL.i1
    String    -> return $ LL.ptr LL.i8
    Tuple ts  -> LL.StructureType True <$> mapM opTypeOf ts
    Array n t -> LL.ArrayType (fromIntegral n) <$> opTypeOf t

    Table [Char] -> do
        return $ LL.NamedTypeReference (mkName "String")

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

    ADT tss
        | isEmptyADT typ -> return $ LL.ptr LL.void
        | isNormalADT typ -> return $ LL.StructureType False [LL.i64, LL.ptr LL.i8]
        | isEnumADT typ -> return $ LL.i64
        | isPtrADT typ -> error ""

    UnsafePtr t -> case t of
        Char -> LL.ptr <$> opTypeOf Char
        _ -> fail $ show t

    _         -> error (show typ) 


