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


storeAdtNull :: InsCmp CompileState m => Pointer -> m ()
storeAdtNull location = do
    ADT fs <- baseTypeOf location
    assert (elem FieldNull fs) "ADT does not have a null field"
    adtSetEnum location $ fromJust $ elemIndex FieldNull fs


adtNullField :: InsCmp CompileState m => Type -> m Int
adtNullField adtType = do 
    ADT fs <- baseTypeOf adtType
    assert (FieldNull `elem` fs) $ "ADT does not contain null"
    return $ fromJust $ elemIndex FieldNull fs


adtTypeField :: InsCmp CompileState m => Type -> Type -> m Int
adtTypeField adtType typ = do
    ADT fs <- baseTypeOf adtType
    assert (FieldType typ `elem` fs) $ "ADT does not contain type field: " ++ show typ
    return $ fromJust $ elemIndex (FieldType typ) fs


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = do
    base <- baseTypeOf adt 
    case base of
        Enum   -> return $ Value I64 (op adt)
        ADT fs -> Value I64 <$> extractValue (op adt) [0]


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
        FieldCtor ts -> do
            pField <- gep (loc adt) [int32 0, int32 1]
            pType <- bitcast pField . LL.ptr =<< opTypeOf (Tuple ts)
            return $ Pointer (Tuple ts) pType
        _ -> error $ "adtField: " ++ show (fs !! i)
            
