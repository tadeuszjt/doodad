{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Typeof where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
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
    Typedef sym -> trace ("baseTypeOf " ++ show sym) $ do ObjType t <- look sym; baseTypeOf t
    _           -> return typ


sizeOf :: InsCmp CompileState m => Type -> m Value
sizeOf typ = trace "sizeOf" $ do
    opType <- opTypeOf typ
    return $ Val I64 $ cons $ C.SExt (C.sizeof opType) (LL.IntegerType 64)

sizeOfLL :: ModCmp CompileState m => LL.Type -> m Int
sizeOfLL typ = do
    context <- gets context
    dataLayout <- gets dataLayout
    n <- liftIO $ FFI.getTypeAllocSize dataLayout =<< runEncodeAST context (encodeM typ)
    return (fromIntegral n)


isDataType :: InsCmp CompileState m => Type -> m Bool
isDataType typ = do
    base <- baseTypeOf typ
    case base of
        Io                -> return True
        Void              -> return False
        String            -> return False
        UnsafePtr         -> return False
        _ | isSimple base -> return False
        Table ts          -> return True
        Tuple ts          -> any (== True) <$> mapM isDataType ts
        Range t           -> isDataType t
        ADT xs            -> do
            bs <- forM xs $ \x -> case x of
                FieldNull -> return False
                FieldType t -> isDataType t
                FieldCtor ts -> any (== True) <$> mapM isDataType ts
            return $ any (== True) bs
        _                 -> fail (show base)



opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = withErrorPrefix ("opTypOf " ++ show typ) $ case typ of
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
    Enum      -> return LL.i64
    Io        -> return LL.i64 -- no underlying type
    Range t   -> LL.StructureType False <$> mapM opTypeOf [t, t]
    Tuple ts  -> LL.StructureType False <$> mapM opTypeOf ts
    Array n t -> LL.ArrayType (fromIntegral n) <$> opTypeOf t
    ADT fs    -> do
        types <- forM fs $ \f -> case f of
            FieldNull -> return I8
            FieldType t -> return t
            FieldCtor ts -> return $ Tuple ts

        sizes <- mapM sizeOfLL =<< mapM opTypeOf types
        let maxi = fromJust $ elemIndex (maximum sizes) sizes
        opTypeOf $ Tuple [I64, types !! maxi]

    Table ts  -> do
        ps <- map LL.ptr <$> mapM opTypeOf ts
        return $ LL.StructureType False (LL.i64:LL.i64:ps)

    Sparse ts  -> do
        stack <- opTypeOf (Table [I64])
        table <- opTypeOf (Table ts)
        return $ LL.StructureType False [table, stack]

    Typedef s -> do
        ObjType t <- look s
        namem <- Map.lookup (Typedef s) <$> gets typeNameMap
        maybe (opTypeOf t) (return . LL.NamedTypeReference) namem

    Func ts rt -> do
        rt' <- opTypeOf rt
        ts' <- mapM opTypeOf ts
        return $ LL.ptr (LL.FunctionType rt' ts' False)

    UnsafePtr -> return $ LL.ptr LL.VoidType

    _         -> error (show typ) 


