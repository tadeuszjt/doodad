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


adtHasNull :: InsCmp CompileState m => Type -> m (Maybe Int)
adtHasNull typ = do 
    ADT fs <- assertBaseType isADT typ
    let is = elemIndices FieldNull fs
    assert (length is <= 1) "ADT does not have a unique null field"
    case is of
        [i] -> return (Just i)
        _   -> return Nothing


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull adtTyp = do
    assertBaseType isADT adtTyp
    im <- adtHasNull adtTyp
    assert (isJust im) "ADT does not have a null field"

    adt <- mkAlloca adtTyp
    adtSetEnum adt (fromJust im)
    return adt


-- construct ADT from a value.
-- E.g. SomeAdt(4:i64), SomeAdt must have i64 field
-- This function will use the location, must be allocated beforehand.
adtConstructFromPtr :: InsCmp CompileState m => Type -> Value -> m Value
adtConstructFromPtr adtTyp loc@(Ptr _ _) = do
    base@(ADT tss) <- assertBaseType isADT adtTyp

    case adtTyp of
        Typedef symbol -> do -- can lookup member
            ObjField i <- look symbol (KeyTypeField $ valType loc)
            adt <- mkAlloca adtTyp
            adtSetEnum adt i
            adtSetPi8 adt =<< bitcast (valLoc loc) (LL.ptr LL.i8)
            return adt

        ADT fs -> do
            let is = elemIndices (FieldType $ valType loc) fs
            assert (length is == 1) "ADT doe not have unique type field"
            adt <- mkAlloca adtTyp
            adtSetEnum adt (head is)
            adtSetPi8 adt =<< bitcast (valLoc loc) (LL.ptr LL.i8)
            return adt



adtTypeDef :: InsCmp CompileState m => Symbol -> AST.AnnoType -> m ()
adtTypeDef symbol (AST.AnnoADT xs) = trace "adtTypeDef" $ do
    let typdef = Typedef symbol

    fs <- forM xs $ \x -> case x of
        AST.ADTFieldMember symbol ts -> return $ FieldCtor ts
        AST.ADTFieldType t           -> return $ FieldType t
        AST.ADTFieldNull             -> return FieldNull

    define symbol KeyType $ ObjType (ADT fs)

    -- define member loopkups
    forM_ (zip xs [0..]) $ \(x, i) -> case x of
        AST.ADTFieldMember s ts -> do
            define s KeyFunc (ObjField i)
            define s (KeyField typdef) (ObjField i)
        AST.ADTFieldType t@(Typedef s) -> do
            define s (KeyField typdef) (ObjAdtTypeField i)
            define symbol (KeyTypeField t) (ObjField i)
        AST.ADTFieldType t -> do
            define symbol (KeyTypeField t) (ObjField i)
        AST.ADTFieldNull -> return ()

    -- define constructors
    define symbol KeyFunc ObjCtor


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = trace "adtEnum" $ do
    assertBaseType isADT (valType adt)
    val <- valLoad adt
    Val I64 <$> extractValue (valOp val) [0]


adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt@(Ptr _ loc) i = trace "adtSetEnum" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    assert (i >= 0 && i < length tss) "invalid ADT enum"
    en <- Ptr I64 <$> gep loc [int32 0, int32 0]
    valStore en (mkI64 i)


adtDeref :: InsCmp CompileState m => Value -> Int -> Int -> m Value
adtDeref adt i j = trace "adtDeref" $ do
    ADT fs <- assertBaseType isADT (valType adt)
    ptr <- adtPi8 adt
    case fs !! i of
        FieldNull -> fail "invalid adt deref"
        FieldType t -> do -- ptr points directly to val
            assert (j == 0) "invalid ADT deref"
            fmap (Ptr t) . bitcast ptr . LL.ptr =<< opTypeOf t
        FieldCtor ts -> do
            assert (j < length ts) "invalid adt deref"
            ptr <- adtPi8 adt
            ptup <- bitcast ptr . LL.ptr =<< opTypeOf (Tuple ts)
            ptrTupleIdx j (Ptr (Tuple ts) ptup)


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 adt = trace "adtPi8" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    op <- valOp <$> valLoad adt
    extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 adt@(Ptr _ loc) pi8 = trace "adtSetPi8" $ do
    ADT tss <- assertBaseType isADT (valType adt)
    ppi8 <- gep loc [int32 0, int32 1]
    store ppi8 0 pi8


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => Symbol -> Type -> [Value] -> m Value
adtConstructField symbol typ vals = do
    ADT fs <- assertBaseType isADT typ
    ObjField i <- look symbol (KeyField typ)
    adt <- mkAlloca typ
    adtSetEnum adt i
    case fs !! i of
        FieldType t -> do
            assert (length vals == 1) "Invalid ADT constructor arguments"
            assert (map valType vals == [t]) "mismatch types"
            mal <- mkMalloc t (mkI64 1)
            valStore mal (head vals)
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
        FieldCtor ts -> do
            assert (length vals == length ts) "Invalid ADT constructor arguments"
            assert (map valType vals == ts) "mismatch types"
            tup <- mkMalloc (Tuple ts) (mkI64 1)
            forM_ (zip vals [0..]) $ \(val, j) -> do
                ptr <- ptrTupleIdx j tup
                valStore ptr val
            adtSetPi8 adt =<< bitcast (valLoc tup) (LL.ptr LL.i8)

    return adt
