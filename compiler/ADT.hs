{-# LANGUAGE FlexibleContexts #-}
module ADT where

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


-- ADT type rules:
-- ADT []         *i8
-- ADT [(s, f64)] *f64
-- ADT [(s, f64), (t, i64)] struct { enum type, i8* }


adtEnum :: InsCmp CompileState m => Value -> m Value
adtEnum adt = do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> err "ADT has no enum"
        _ | isPtrADT adtTyp    -> err "ADT has no enum"
        _ | isEnumADT adtTyp   -> do
            val <- valLoad adt
            return $ val { valType = I64 }
        _ | isNormalADT adtTyp -> do
            val <- valLoad adt
            Val I64 <$> extractValue (valOp val) [0]


adtSetEnum :: InsCmp CompileState m => Value -> Int -> m ()
adtSetEnum adt@(Ptr _ loc) i = do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    assert (i >= 0 && i < length xs) "invalid ADT enum"
    case adtTyp of
        _ | isEmptyADT adtTyp  -> err "ADT has no enum"
        _ | isPtrADT adtTyp    -> err "ADT has no enum"
        _ | isEnumADT adtTyp   -> valStore adt (valI64 i)
        _ | isNormalADT adtTyp -> do
            en <- Ptr I64 <$> gep loc [int32 0, int32 0]
            valStore en (valI64 i)


adtDeref :: InsCmp CompileState m => Value -> m Value
adtDeref val = do
    ADT [(s, t)] <- assertBaseType isPtrADT (valType val)
    pi8 <- adtPi8 val
    pt  <- LL.ptr <$> opTypeOf t
    Ptr t <$> bitcast pi8 pt


adtNull :: InsCmp CompileState m => Type -> m Value
adtNull typ = do
    ADT xs <- assertBaseType isADT typ
    let is = [ i | (("", Void), i) <- zip xs [0..] ]
    assert (length is == 1) (show typ ++ " does not have a unique null constructor")

    loc <- valLocal typ
    when (length xs > 1) $ adtSetEnum loc (head is)
    return loc


adtPi8 :: InsCmp CompileState m => Value -> m LL.Operand
adtPi8 adt = do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    op <- valOp <$> valLoad adt
    case adtTyp of
        _ | isEmptyADT adtTyp -> err "Empty ADT has no pointer"
        _ | isEnumADT adtTyp  -> err "Empty ADT has no pointer"
        _ | isPtrADT adtTyp   -> return op
        _ | isNormalADT adtTyp -> extractValue op [1]


adtSetPi8 :: InsCmp CompileState m => Value -> LL.Operand -> m ()
adtSetPi8 adt@(Ptr _ loc) pi8 = do
    adtTyp@(ADT xs) <- assertBaseType isADT (valType adt)
    case adtTyp of
        _ | isEmptyADT adtTyp  -> err "Empty ADT has no pointer"
        _ | isEnumADT adtTyp   -> err "Empty ADT has no pointer"
        _ | isPtrADT adtTyp    -> store loc 0 pi8
        _ | isNormalADT adtTyp -> do
            ppi8 <- gep loc [int32 0, int32 1]
            store ppi8 0 pi8


-- Construct a specific ADT field, eg: TokSym("ident")
adtConstructField :: InsCmp CompileState m => String -> Type -> [Value] -> m Value
adtConstructField sym typ vals = do
    adtTyp@(ADT xs) <- assertBaseType isADT typ
    adt <- valLocal typ

    case adtTyp of
        _ | isEmptyADT adtTyp  -> assert (length vals == 0) "Invalid ADT constructor arguments"

        _ | isEnumADT adtTyp   -> do
            assert (length vals == 0) "Invalid ADT constructor arguments"
            let idxs = [ i | (s, i) <- zip (map fst xs) [0..], s == sym ]
            assert (length idxs == 1) "Invalid or ambiguous ADT constructor"
            adtSetEnum adt (head idxs)

        _ | isPtrADT adtTyp -> do
            assert (length vals == 1) "Invalid ADT constructor arguments"
            let [(s, t)] = xs
            let [val] = vals
            assert (s == sym) "Invalid ADT constructor"
            assert (t == valType val) "Invalid ADT argument type"
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)

        _ | isNormalADT adtTyp -> do
            assert (length vals == 1) "Invalid ADT constructor arguments"
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


-- ADT()       -> zero constructor
-- ADT(i64(n)) -> construct from unique type field
-- ADT(adt2)   -> construct from adt with ONE equivalent field
-- ADT(null)   -> null becomes adt with equivalent field
adtConstruct :: InsCmp CompileState m => Type -> Value -> m Value
adtConstruct adtTyp (Exp (S.Null _)) = adtNull adtTyp
adtConstruct adtTyp (Exp _)          = error "adt constructing from contextual"
adtConstruct adtTyp val              = do
    ADT xs  <- assertBaseType isADT adtTyp

    let is = [ i | ((s, t), i) <- zip xs [0..], t == valType val, s == "" ]
    if length is == 1 then do -- has unique type field
        let i = head is
        adt <- valLocal adtTyp
        adtSetEnum adt i
        mal <- valMalloc (valType val) (valI64 1)
        valStore mal val
        adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
        return adt
    else do                   -- must be another adt
        ADT xs' <- assertBaseType isADT (valType val)
        assert (length xs' == 1) "Argument ADT must have one field"
        assert (head xs' `elem` xs) "Argument ADT does have equivalent field"
        let x@(s, t) = head xs'

        adt <- valLocal adtTyp
        adtSetEnum adt $ fromJust (elemIndex x xs)

        if t == Void then return adt
        else do
            mal <- valMalloc t (valI64 1)
            valStore mal =<< adtDeref val
            adtSetPi8 adt =<< bitcast (valLoc mal) (LL.ptr LL.i8)
            return adt

