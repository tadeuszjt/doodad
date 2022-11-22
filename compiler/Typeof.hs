{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Typeof where

import Data.Maybe
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
import LLVM.IRBuilder.Module

import Type
import Monad
import State
import Funcs
import Trace
import Error
import Symbol

assertBaseType :: InsCmp CompileState m => (Type -> Bool) -> Type -> m Type
assertBaseType f typ = trace "assertBaseType" $ do
    base <- baseTypeOf typ
    assert (f base) ("Invalid base type of: " ++ show typ)
    return base


baseTypeOf :: ModCmp CompileState m => Type -> m Type
baseTypeOf typ = case typ of
    Typedef sym -> do ObType t <- look sym KeyType; baseTypeOf t
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


getTypeName :: ModCmp CompileState m => Type -> m (Maybe Name)
getTypeName typ = do
    resm <- Map.lookup typ <$> gets typeMap
    when (isNothing resm) $ do
        base <- baseTypeOf typ
        case base of
            Tuple ts -> do
                name <- case typ of
                    Typedef (SymResolved m s i) -> return $ mkName (m ++ "." ++ s)
                    _              -> myFreshPrivate "Tuple"

                typedef name . Just . LL.StructureType True =<< mapM opTypeOf ts
                modify $ \s -> s { typeMap = Map.insert typ name (typeMap s) }

            Table ts -> case typ of
                Typedef (SymResolved m s i) -> do
                    let name = mkName (m ++ "." ++ s)
                    ps <- map LL.ptr <$> mapM opTypeOf ts
                    let opType = LL.StructureType False (LL.i64:LL.i64:ps)
                    typedef name (Just opType)
                    modify $ \s -> s { typeMap = Map.insert typ name (typeMap s) }
                _ -> return ()


            _ -> return ()

    Map.lookup typ <$> gets typeMap


opTypeOf :: ModCmp CompileState m => Type -> m LL.Type
opTypeOf typ = do
    namem <- getTypeName typ
    case namem of
        Just name -> return $ LL.NamedTypeReference name
        Nothing -> case typ of
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

            Table ts  -> do
                ps <- map LL.ptr <$> mapM opTypeOf ts
                return $ LL.StructureType False (LL.i64:LL.i64:ps)

            Sparse ts  -> do
                stack <- opTypeOf (Table [I64])
                table <- opTypeOf (Table ts)
                return $ LL.StructureType False [table, stack]

            Typedef s -> do
                ObType t <- look s KeyType
                opTypeOf t

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


valZero :: InsCmp CompileState m => Type -> m Value
valZero typ = trace ("valZero " ++ show  typ) $ do
    namem <- getTypeName typ
    base <- baseTypeOf typ
    case base of
        I64       -> return $ Val typ (int64 0)
        Bool      -> return $ Val typ (bit 0)
        Array n t -> Val typ . array . replicate n . toCons . valOp <$> valZero t
        Tuple ts  -> Val typ . struct namem True . map (toCons . valOp) <$> mapM valZero ts

        Table ts -> do
            let zi64 = toCons (int64 0)
            zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
            return $ Val typ $ struct namem False (zi64:zi64:zptrs)

        Sparse ts -> do
            stack <- valOp <$> valZero (Table [I64])
            table <- valOp <$> valZero (Table ts)
            return $ Val typ $ struct namem False [toCons table, toCons stack]
                                
        _ -> error ("valZero: " ++  show typ)

