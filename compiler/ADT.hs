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

import qualified AST as S
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

    adt <- valLocal adtTyp
    adtSetEnum adt (fromJust im)
    return adt


-- construct ADT from a value.
-- E.g. SomeAdt(4:i64), SomeAdt must have i64 field
-- This function will use the location, must be allocated beforehand.
adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct adtTyp loc@(Ptr _ _) = do
    base@(ADT tss) <- assertBaseType isADT adtTyp

    case adtTyp of
        Typedef symbol -> do -- can lookup member
            ObjMember i <- look symbol (KeyTypeField $ valType loc)
            adt <- valLocal adtTyp
            adtSetEnum adt i
            pi8 <- bitcast (valLoc loc) (LL.ptr LL.i8)
            adtSetPi8 adt pi8
            return adt

    


adtTypeDef :: InsCmp CompileState m => Symbol -> S.AnnoType -> m ()
adtTypeDef symbol (S.AnnoADT xs) = trace "adtTypeDef" $ do
    let typdef = Typedef symbol

    fs <- forM xs $ \x -> case x of
        S.ADTFieldMember symbol ts -> return $ FieldCtor ts
        S.ADTFieldType t           -> return $ FieldType t
        S.ADTFieldNull             -> return FieldNull

    namem <- case ADT fs of
        _ | isNormalADT (ADT fs) -> do
            name <- addTypeDef symbol =<< opTypeOf (ADT fs)
            return (Just name)
        _ | otherwise -> return Nothing

    define symbol KeyType $ ObType (ADT fs) namem

    -- define member loopkups
    forM_ (zip xs [0..]) $ \(x, i) -> case x of
        S.ADTFieldMember s ts -> do
            define s (KeyFunc ts typdef) (ObjMember i)
            define s (KeyMember typdef) (ObjMember i)
        S.ADTFieldType t@(Typedef s) -> do
            define s (KeyMember typdef) (ObjAdtTypeMember i)
            define symbol (KeyTypeField t) (ObjMember i)
        S.ADTFieldType t -> do
            define symbol (KeyTypeField t) (ObjMember i)
        S.ADTFieldNull -> return ()

    -- define constructors
    define symbol (KeyFunc [] typdef)       ObjConstructor 
    define symbol (KeyFunc [typdef] typdef) ObjConstructor
    forM_ xs $ \x -> case x of
        S.ADTFieldType t -> define symbol (KeyFunc [t] typdef) ObjConstructor
        _ -> return ()


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = trace "adtEnum" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "ADT has no enum"
        --_ | isPtrADT adtTyp    -> fail "ADT has no enum"
        --
        _ | isEnumADT adtTyp   -> do
            Val _ op <- valLoad adt
            return $ Val I64 op

        _ | isNormalADT adtTyp -> do
            val <- valLoad adt
            Val I64 <$> extractValue (valOp val) [0]


adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt@(Ptr _ loc) i = trace "adtSetEnum" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    assert (i >= 0 && i < length tss) "invalid ADT enum"
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "ADT has no enum"
        --_ | isPtrADT adtTyp    -> fail "ADT has no enum"
        _ | isEnumADT adtTyp   -> do
            Val I64 iop <- valInt I64 i
            valStore adt (Val adtTyp iop)
        _ | isNormalADT adtTyp -> do
            en <- Ptr I64 <$> gep loc [int32 0, int32 0]
            valStore en (valI64 i)


adtDeref :: InsCmp CompileState m => Value -> Int -> Int -> m Value
adtDeref adt i j = trace "adtDeref" $ do
    base@(ADT fs) <- assertBaseType isADT (valType adt)
    case base of
        _ | isEnumADT base -> fail "Cannot deref enum ADT"
        _ | isEmptyADT base -> fail "Cannot deref empty ADT"
        _ | isNormalADT base -> do
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
                    tupleIdx j (Ptr (Tuple ts) ptup)



            
adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 adt = trace "adtPi8" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    op <- valOp <$> valLoad adt
    case adtTyp of
        _ | isEmptyADT adtTyp -> fail "Empty ADT has no pointer"
        _ | isEnumADT adtTyp  -> fail "Empty ADT has no pointer"
        --_ | isPtrADT adtTyp   -> return op
        _ | isNormalADT adtTyp -> extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 adt@(Ptr _ loc) pi8 = trace "adtSetPi8" $ do
    adtTyp@(ADT tss) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "Empty ADT has no pointer"
        _ | isEnumADT adtTyp   -> fail "Empty ADT has no pointer"
        --_ | isPtrADT adtTyp    -> store loc 0 pi8
        _ | isNormalADT adtTyp -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => Symbol -> Type -> [Value] -> m Value
adtConstructField symbol typ vals = trace ("adtConstructField " ++ show symbol) $ do
    adtTyp@(ADT fs) <- assertBaseType isADT typ

    case adtTyp of
--        _ | isEmptyADT adtTyp  -> do
--            assert (length vals == 0) "Invalid ADT constructor arguments"
--            valZero typ

        _ | isEnumADT adtTyp   -> do
            ObjMember i <- look symbol (KeyMember typ)
            assert (length vals == 0) "Invalid ADT constructor arguments"
            adt <- valLocal typ
            adtSetEnum adt i
            return adt


        _ | isNormalADT adtTyp -> do
            ObjMember i <- look symbol (KeyMember typ)
            adt <- valLocal typ
            adtSetEnum adt i
            case fs !! i of
                FieldType t -> do
                    assert (length vals == 1) "Invalid ADT constructor arguments"
                    assert (map valType vals == [t]) "mismatch types"
                    mal <- valMalloc t (valI64 1)
                    valStore mal (head vals)
                    adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
                FieldCtor ts -> do
                    assert (length vals == length ts) "Invalid ADT constructor arguments"
                    assert (map valType vals == ts) "mismatch types"
                    tup <- valMalloc (Tuple ts) (valI64 1)
                    forM_ (zip vals [0..]) $ \(val, j) -> do
                        tupleSet tup j val
                    adtSetPi8 adt =<< bitcast (valLoc tup) (LL.ptr LL.i8)

            return adt

--        _ | isPtrADT adtTyp -> do
--            ObjMember i <- look symbol (KeyMember typ)
--            let ts = tss !! i
--            assert (length vals == 1) "Invalid ADT constructor arguments"
--            assert (length ts == 1) "Invalid ADT constructor args"
--            let t = head ts
--            let [val] = vals
--            checkTypesCompatible (valType val) t
--
--            adt <- valLocal typ
--            mal <- valMalloc t (valI64 1)
--            valStore mal val
--            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
--            return adt

--adtNull :: InsCmp CompileState m => Type -> m Value
--adtNull typ = trace "adtNull" $ do
--    ADT tss <- assertBaseType isADT typ
--    assert (length is == 1) (show typ ++ " does not have a unique null constructor")
--
--    loc <- valLocal typ
--    when (length ts > 1) $ adtSetEnum loc (head is)
--    return loc


--
--        _ | isNormalADT adtTyp && length vals == 0 -> do
--            ObjMember i <- look symbol (KeyMember typ)
--            assert (ts !! i == Void) "Invalid ADT constructor."
--            adt <- valLocal typ
--            adtSetEnum adt i
--            return adt
--
--        _ | isNormalADT adtTyp && length vals > 1 -> do
--            ObjMember i <- look symbol (KeyMember typ)
--            Tuple tts <- assertBaseType isTuple (ts !! i)
--            mal <- valMalloc (ts !! i) (valI64 1)
--
--            assert (length vals == length tts) "Invalid ADT constructor"
--            zipWithM_ checkTypesCompatible (map valType vals) tts
--            zipWithM_ (tupleSet mal) [0..] vals
--
--            adt <- valLocal typ
--            adtSetEnum adt i
--            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
--            return adt
--
--
--            
--adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
--adtConstruct typ val = trace "adtConstruct" $ do
--    adtTyp@(ADT ts) <- assertBaseType isADT typ
--    case adtTyp of
--        _ | isEmptyADT adtTyp -> fail "Cannot construct ADT type"
--
--        _ | isEnumADT adtTyp -> fail "here"
--
--        _ | isPtrADT adtTyp -> fail "here"
--
--        _ | isNormalADT adtTyp -> do
--            let idxs = elemIndices (valType val) ts
--            assert (length idxs == 1) "Ambiguous or invalid ADT type constructor"
--            let idx = head idxs
--            adt <- valLocal typ
--            adtSetEnum adt idx
--            mal <- valMalloc (valType val) (valI64 1)
--            valStore mal val
--            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
--            return adt
