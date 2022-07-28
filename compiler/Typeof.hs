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


checkTypesCompatible :: InsCmp CompileState m => Type -> Type -> m ()
checkTypesCompatible typA typB = do
    baseA <- baseTypeOf typA
    baseB <- baseTypeOf typB
    case baseA of
        t | isSimple t -> assert (baseA == baseB) $
            "Types " ++ show typA ++ " and " ++ show typB ++ " aren't compatible"

        t | isTuple t  -> do
            assertBaseType isTuple baseB
            let Tuple ats = baseA
            let Tuple bts = baseB
            assert (length ats == length bts) "Tuples aren't compatible"
            zipWithM_ checkTypesCompatible ats bts

        t | isTable t -> do
            assertBaseType isTable baseB
            let Table ats = baseA
            let Table bts = baseB
            assert (length ats == length bts) "Tables aren't compatible"
            zipWithM_ checkTypesCompatible ats bts

        t | isArray t -> do
            assertBaseType isArray baseB
            let Array na ta = baseA
            let Array nb tb = baseB
            assert (na == nb) "array length mismatch"
            checkTypesCompatible ta tb

        t | isFunc t -> do
            assertBaseType isFunc baseB
            let Func ats atr = baseA
            let Func bts btr = baseB
            checkTypesCompatible atr btr
            assert (length ats == length bts) "Funcs aren't compatible"
            zipWithM_ checkTypesCompatible ats bts

        t | isADT t -> do
            assertBaseType isADT baseB
            let ADT atss = baseA
            let ADT btss = baseB
            assert (length atss == length btss) "ADTs aren't compatible"
            forM_ (zip atss btss) $ \(ats, bts) -> do
                assert (length ats == length bts) "ADTs aren't compatible"
                zipWithM_ checkTypesCompatible ats bts

        UnsafePtr t -> do
            let UnsafePtr ta = baseA
            let UnsafePtr tb = baseB
            checkTypesCompatible ta tb

            
        _ -> fail $ "Can't checkTypesCompatible: " ++ show typA


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
    Tuple ts  -> LL.StructureType False <$> mapM opTypeOf ts
    Array n t -> LL.ArrayType (fromIntegral n) <$> opTypeOf t

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
        | isNormalADT typ -> return $ LL.StructureType False [LL.i64, LL.ptr LL.void]
        | isEnumADT typ -> return $ LL.i64
        | isPtrADT typ -> let [[t]] = tss in fmap LL.ptr (opTypeOf t)

    UnsafePtr t -> case t of
        Char -> LL.ptr <$> opTypeOf Char
        _ -> fail $ show t

    _         -> error (show typ) 


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef sym -> trace ("baseTypeOf " ++ show sym) $ do ObType t _ <- look sym KeyType; baseTypeOf t
    _           -> return typ


sizeOf :: InsCmp CompileState m => Type -> m Value
sizeOf typ = trace "sizeOf" $ do
    opType <- opTypeOf typ
    return $ Val I64 $ cons $ C.SExt (C.sizeof opType) (LL.IntegerType 64)
