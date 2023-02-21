{-# LANGUAGE FlexibleContexts #-}
module ADT where

import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Control.Monad

import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST
import Value
import Type
import State
import Monad
import Funcs
import Typeof
import Trace
import Tuple
import Error
import Symbol


adtNull :: InsCmp CompileState m => Type -> m Pointer
adtNull typ = do
    ADT fs <- baseTypeOf typ
    assert (elem FieldNull fs) "ADT does not have a null field"
    adt <- newVal typ
    adtSetEnum adt $ fromJust $ elemIndex FieldNull fs
    return adt


adtTypeField :: InsCmp CompileState m => Type -> Type -> m Int
adtTypeField adtType typ = do
    ADT fs <- baseTypeOf adtType
    assert (FieldType typ `elem` fs) $ "ADT does not contain type field: " ++ show typ
    return $ fromJust $ elemIndex (FieldType typ) fs


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = do
    ADT fs <- baseTypeOf adt
    Value I64 <$> extractValue (op adt) [0]


adtSetEnum :: InsCmp CompileState m => Pointer -> Int -> m ()
adtSetEnum adt i = do
    ADT fs <- baseTypeOf adt
    assert (i >= 0 && i < length fs) "ADT enum value out of range"
    pi64 <- gep (loc adt) [int32 0, int32 0]
    store pi64 0 (int64 $ fromIntegral i)


adtField :: InsCmp CompileState m => Pointer -> Int -> m Pointer
adtField adt i = do
    ADT fs <- baseTypeOf adt
    case fs !! i of
        FieldType typ -> do
            pField <- gep (loc adt) [int32 0, int32 1]
            pType <- bitcast pField . LL.ptr =<< opTypeOf typ
            return $ Pointer typ pType
        FieldCtor [t] -> do
            pField <- gep (loc adt) [int32 0, int32 1]
            pType <- bitcast pField . LL.ptr =<< opTypeOf t
            return $ Pointer t pType
        _ -> error $ "adtField: " ++ show (fs !! i)
            
        
adtDeref :: InsCmp CompileState m => Pointer -> Int -> Int -> m Pointer
adtDeref adt i j = trace "adtDeref" $ do
    ADT fs <- baseTypeOf adt
    case fs !! i of
        FieldNull -> fail "invalid adt deref"
        FieldType t -> do
            assert (j == 0) "invalid ADT deref"
            adtField adt i
        FieldCtor [t] -> do
            assert (j == 0) "invalid ADT deref"
            adtField adt i

