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


adtTypeDef :: InsCmp CompileState m => String -> S.AnnoType -> m ()
adtTypeDef sym anno = trace "adtTypeDef" $ do
    let typdef = Typedef (Sym sym)
    define sym (KeyFunc [])       (ObjConstructor typdef)
    define sym (KeyFunc [typdef]) (ObjConstructor typdef)

    case anno of
        S.AnnoADT xs -> do
            define sym KeyType $ ObType (ADT $ map snd xs) Nothing
            forM_ (zip xs [0..]) $ \((s, t), i) -> do
                define s (KeyMember typdef) (ObjMember i)
                define s (KeyFunc [t]) (ObjADTFieldCons typdef)
                define s (KeyFunc []) (ObjADTFieldCons typdef)
                when (isTuple t) $ do
                    let Tuple ts = t
                    define s (KeyFunc ts) (ObjADTFieldCons typdef)
            


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = trace "adtEnum" $ do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "ADT has no enum"
        _ | isPtrADT adtTyp    -> fail "ADT has no enum"
        _ | isEnumADT adtTyp   -> do
            val <- valLoad adt
            return $ val { valType = I64 }
        _ | isNormalADT adtTyp -> do
            val <- valLoad adt
            Val I64 <$> extractValue (valOp val) [0]


adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt@(Ptr _ loc) i = trace "adtSetEnum" $ do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    assert (i >= 0 && i < length xs) "invalid ADT enum"
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "ADT has no enum"
        _ | isPtrADT adtTyp    -> fail "ADT has no enum"
        _ | isEnumADT adtTyp   -> valStore adt $ (valI64 i) { valType = adtTyp }
        _ | isNormalADT adtTyp -> do
            en <- Ptr I64 <$> gep loc [int32 0, int32 0]
            valStore en (valI64 i)


adtDeref :: InsCmp CompileState m => Value -> m Value
adtDeref val = trace "adtDeref" $ do
    ADT [t] <- assertBaseType isPtrADT (valType val)
    pi8 <- adtPi8 val
    pt  <- LL.ptr <$> opTypeOf t
    Ptr t <$> bitcast pi8 pt


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull typ = trace "adtNull" $ do
    ADT ts <- assertBaseType isADT typ
    let is = elemIndices Void ts
    assert (length is == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length ts > 1) $ adtSetEnum loc (head is)
    return loc


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 adt = trace "adtPi8" $ do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    op <- valOp <$> valLoad adt
    case adtTyp of
        _ | isEmptyADT adtTyp -> fail "Empty ADT has no pointer"
        _ | isEnumADT adtTyp  -> fail "Empty ADT has no pointer"
        _ | isPtrADT adtTyp   -> return op
        _ | isNormalADT adtTyp -> extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 adt@(Ptr _ loc) pi8 = trace "adtSetPi8" $ do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> fail "Empty ADT has no pointer"
        _ | isEnumADT adtTyp   -> fail "Empty ADT has no pointer"
        _ | isPtrADT adtTyp    -> store loc 0 pi8
        _ | isNormalADT adtTyp -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
adtConstructField sym typ vals = trace ("adtConstructField " ++ sym) $ do
    adtTyp@(ADT ts) <- assertBaseType isADT typ

    case adtTyp of
        _ | isEmptyADT adtTyp  -> do
            assert (length vals == 0) "Invalid ADT constructor arguments"
            valZero typ

        _ | isEnumADT adtTyp   -> do
            ObjMember i <- look (Sym sym) (KeyMember typ)
            assert (length vals == 0) "Invalid ADT constructor arguments"
            adt <- valLocal typ
            adtSetEnum adt i
            return adt

        _ | isPtrADT adtTyp -> do
            ObjMember i <- look (Sym sym) (KeyMember typ)
            
            assert (length vals == 1) "Invalid ADT constructor arguments"
            let [val] = vals
            checkTypesCompatible (valType val) (ts !! i)

            adt <- valLocal typ
            mal <- valMalloc (ts !! i) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt

        _ | isNormalADT adtTyp && length vals == 1 -> do
            ObjMember i <- look (Sym sym) (KeyMember typ)

            let [val] = vals
            checkTypesCompatible (valType val) (ts !! i)
            mal <- valMalloc (ts !! i) (valI64 1)
            valStore mal val

            adt <- valLocal typ
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            adtSetEnum adt i
            return adt

        _ | isNormalADT adtTyp && length vals == 0 -> do
            ObjMember i <- look (Sym sym) (KeyMember typ)
            assert (ts !! i == Void) "Invalid ADT constructor."
            adt <- valLocal typ
            adtSetEnum adt i
            return adt

        _ | isNormalADT adtTyp && length vals > 1 -> do
            ObjMember i <- look (Sym sym) (KeyMember typ)
            Tuple tts <- assertBaseType isTuple (ts !! i)
            mal <- valMalloc (ts !! i) (valI64 1)

            assert (length vals == length tts) "Invalid ADT constructor"
            zipWithM_ checkTypesCompatible (map valType vals) tts
            zipWithM_ (tupleSet mal) [0..] vals

            adt <- valLocal typ
            adtSetEnum adt i
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt


            
adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct typ val = trace "adtConstruct" $ do
    adtTyp@(ADT ts) <- assertBaseType isADT typ
    case adtTyp of
        _ | isEmptyADT adtTyp -> fail "Cannot construct ADT type"

        _ | isEnumADT adtTyp -> fail "here"

        _ | isPtrADT adtTyp -> fail "here"

        _ | isNormalADT adtTyp -> do
            let idxs = elemIndices (valType val) ts
            assert (length idxs == 1) "Ambiguous or invalid ADT type constructor"
            let idx = head idxs
            adt <- valLocal typ
            adtSetEnum adt idx
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt
