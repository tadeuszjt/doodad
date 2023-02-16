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


mkAdtNull :: InsCmp CompileState m => Type -> m Value
mkAdtNull typ = do
    ADT fs <- assertBaseType isADT typ
    assert (elem FieldNull fs) "ADT does not have a null field"
    adt <- newVal typ
    adtSetEnum (fromPointer adt) $ fromJust $ elemIndex FieldNull fs
    return (fromPointer adt)



adtTypeField :: InsCmp CompileState m => Type -> Type -> m Int
adtTypeField adtType typ = do
    ADT fs <- assertBaseType isADT adtType
    assert (FieldType typ `elem` fs) $ "ADT does not contain type field: " ++ show typ
    return $ fromJust $ elemIndex (FieldType typ) fs


mkAdtEnum :: InsCmp CompileState m => Value -> m Value
mkAdtEnum adt = do
    assertBaseType isADT (valType adt)
    val <- valLoad adt
    Val I64 <$> extractValue (valOp val) [0]


adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt i = do
    ADT fs <- assertBaseType isADT (valType adt)
    assert (isPtr adt) "ADT must be a Ptr"
    assert (i >= 0 && i < length fs) "ADT enum value out of range"
    pi64 <- gep (valLoc adt) [int32 0, int32 0]
    store pi64 0 (int64 $ fromIntegral i)


ptrAdtField :: InsCmp CompileState m => Value -> Int -> m Value
ptrAdtField adt i = do
    ADT fs <- assertBaseType isADT (valType adt)
    assert (isPtr adt) "ADT must be a Ptr"
    case fs !! i of
        FieldType typ -> do
            pField <- gep (valLoc adt) [int32 0, int32 1]
            pType <- bitcast pField . LL.ptr =<< opTypeOf typ
            return $ Ptr typ pType
            
        
adtDeref :: InsCmp CompileState m => Value -> Int -> Int -> m Value
adtDeref adt i j = trace "adtDeref" $ do
    ADT fs <- assertBaseType isADT (valType adt)
    case fs !! i of
        FieldNull -> fail "invalid adt deref"
        FieldType t -> do
            assert (j == 0) "invalid ADT deref"
            ptrAdtField adt i

