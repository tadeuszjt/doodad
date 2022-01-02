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


adtTypeDef :: InsCmp CompileState m => String -> Type -> m ()
adtTypeDef sym typ = trace "adtTypeDef" $ do
    assert (isADT typ) "Isn't ADT"
    let ADT xs = typ
    let typdef = Typedef (Sym sym)
    assert (length (Set.fromList xs) == length xs) "ADT fields must be unique"

    addObjWithCheck sym KeyType $ ObType typ Nothing

    -- Add zero, base and def constructors
    addObjWithCheck sym (KeyFunc []) (ObjConstructor typdef)
    addObjWithCheck sym (KeyFunc [typ]) (ObjConstructor typdef)
    addObjWithCheck sym (KeyFunc [typdef]) (ObjConstructor typdef)

    case typ of
        _ | isEmptyADT typ  -> fail "empty"

        _ | isEnumADT typ   ->
            forM_ xs $ \(s, Void) ->
                addObjWithCheck s KeyVar (ObjADTFieldCons typdef)
                
        _ | isPtrADT typ    -> fail "ptr"

        _ | isNormalADT typ ->
            forM_ xs $ \(s, t) -> do
                case t of
                    Void      -> addObjWithCheck s KeyVar (ObjADTFieldCons typdef)
                    Tuple ts -> do
                        addObjWithCheck s (KeyFunc [t]) (ObjADTFieldCons typdef)
                        addObjWithCheck s (KeyFunc ts) (ObjADTFieldCons typdef)
                    _         -> do
                        addObjWithCheck s (KeyFunc [t]) (ObjADTFieldCons typdef)


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
    ADT [(s, t)] <- assertBaseType isPtrADT (valType val)
    pi8 <- adtPi8 val
    pt  <- LL.ptr <$> opTypeOf t
    Ptr t <$> bitcast pi8 pt


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull typ = trace "adtNull" $ do
    ADT xs <- assertBaseType isADT typ
    let is = [ i | (("", Void), i) <- zip xs [0..] ]
    assert (length is == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length xs > 1) $ adtSetEnum loc (head is)
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
    adtTyp@(ADT xs) <- assertBaseType isADT typ

    case adtTyp of
        _ | isEmptyADT adtTyp  -> do
            assert (length vals == 0) "Invalid ADT constructor arguments"
            valZero typ

        _ | isEnumADT adtTyp   -> do
            adt <- valLocal typ
            assert (length vals == 0) "Invalid ADT constructor arguments"
            let idxs = [ i | (s, i) <- zip (map fst xs) [0..], s == sym ]
            assert (length idxs == 1) "Invalid or ambiguous ADT constructor"
            adtSetEnum adt (head idxs)
            return adt

        _ | isPtrADT adtTyp -> do
            adt <- valLocal typ
            assert (length vals == 1) "Invalid ADT constructor arguments"
            let [(s, t)] = xs
            let [val] = vals
            assert (s == sym) "Invalid ADT constructor"
            assert (t == valType val) "Invalid ADT argument type"
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt

        _ | isNormalADT adtTyp && length vals == 1 -> do
            adt <- valLocal typ

            let idxs = [ i | (s, i) <- zip (map fst xs) [0..], s == sym ]
            assert (length idxs == 1) "Invalid or ambiguous ADT constructor"
            let [idx] = idxs
            let [val] = vals
            assert (valType val == map snd xs !! idx) "Invalid"
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            adtSetEnum adt idx
            return adt

        _ | isNormalADT adtTyp && length vals == 0 -> do
            let idxs = [ i | ((s, Void), i) <- zip xs [0..], s == sym ]
            assert (length idxs == 1) "Invalid or ambiguous ADT constructor"
            let [idx] = idxs

            adt <- valLocal typ
            adtSetEnum adt idx
            return adt

        _ | isNormalADT adtTyp && length vals > 1 -> do
            let idxs = [ i | ((s, t), i) <- zip xs [0..], s == sym ]
            assert (length idxs == 1) "Invalid or ambiguous ADT constructor"
            let [idx] = idxs
            let (s, t) = xs !! idx
            Tuple ts <- assertBaseType isTuple t
            mal <- valMalloc t (valI64 1)

            assert (length vals == length ts) "Invalid ADT constructor"
            zipWithM checkTypesCompatible (map valType vals) ts

            forM (zip vals [0..]) $ \(v, i) ->
                tupleSet mal i v

            adt <- valLocal typ
            adtSetEnum adt idx
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt


            
adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct typ val = trace "adtConstruct" $ do
    adtTyp@(ADT xs) <- assertBaseType isADT typ
    case adtTyp of
        _ | isEmptyADT adtTyp -> fail "Cannot construct ADT type"

        _ | isEnumADT adtTyp -> fail "here"

        _ | isPtrADT adtTyp -> fail "here"

        _ | isNormalADT adtTyp -> do
            let idxs = [ i | (t, i) <- zip (map snd xs) [0..], t == valType val ]
            assert (length idxs == 1) "Ambiguous or invalid ADT type constructor"
            let idx = head idxs
            adt <- valLocal typ
            adtSetEnum adt idx
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt
