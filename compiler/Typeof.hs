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

        t | isADT t    -> assert (baseA == baseB) $
            "Types " ++ show typA ++ " and " ++ show typB ++ " aren't compatible" 

        t | isTuple t  -> do
            assertBaseType isTuple baseB
            let Tuple axs = baseA
            let Tuple bxs = baseB
            assert (length axs == length bxs) "Tuples aren't compatible"
            forM_ (zip axs bxs) $ \((_, ta), (_, tb)) -> checkTypesCompatible ta tb

        t | isTable t -> do
            assertBaseType isTable baseB
            let Table ats = baseA
            let Table bts = baseB
            assert (length ats == length bts) "Tables aren't compatible"
            forM_ (zip ats bts) $ \(ta, tb) -> checkTypesCompatible ta tb

        t | isFunction t -> do
            assertBaseType isFunction baseB
            let Func ats atr = baseA
            let Func bts btr = baseB
            checkTypesCompatible atr btr
            forM_ (zip ats bts) $ \(ta, tb) -> checkTypesCompatible ta tb
            
        _ -> err $ "Can't checkTypesCompatible: " ++ show typA


opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = trace ("opTypOf " ++ show typ) $ case typ of
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


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef sym -> trace ("baseTypeOf " ++ show sym) $ do ObType t _ <- look sym KeyType; baseTypeOf t
    _           -> return typ


sizeOf :: InsCmp CompileState m => Type -> m Value
sizeOf typ = trace "sizeOf" $ do
    opType <- opTypeOf typ
    return $ Val I64 $ cons $ C.SExt (C.sizeof opType) (LL.IntegerType 64)
