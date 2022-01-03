{-# LANGUAGE FlexibleContexts #-}
module Tuple where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import qualified AST as S
import Type
import State
import Monad
import Value
import Typeof
import Trace
import Error

tupleTypeDef :: InsCmp CompileState m => String -> S.AnnoType -> m ()
tupleTypeDef sym (S.AnnoType typ) = trace "tupleTypeDef" $ do
    Tuple ts <- assertBaseType isTuple typ
    let typdef = Typedef (Sym sym)

    define sym (KeyFunc []) (ObjConstructor typdef)
    define sym (KeyFunc [typ]) (ObjConstructor typdef)
    define sym (KeyFunc [typdef]) (ObjConstructor typdef)
    define sym KeyType $ ObType typ Nothing

    when (length ts > 0) $
        define sym (KeyFunc ts) (ObjConstructor typdef)

tupleTypeDef sym (S.AnnoTuple xs) = trace "tupleTypeDef" $ do
    let typdef = Typedef (Sym sym)
    let tupTyp = Tuple (map snd xs)

    define sym (KeyFunc []) (ObjConstructor typdef)
    define sym (KeyFunc [tupTyp]) (ObjConstructor typdef)
    define sym (KeyFunc [typdef]) (ObjConstructor typdef)
    define sym KeyType $ ObType tupTyp Nothing

    when (length xs > 0) $ do
        define sym (KeyFunc $ map snd xs) (ObjConstructor typdef)

    forM_ (zip xs [0..]) $ \((s, t), i) -> do
        define s (KeyMember typdef) (ObjMember i)


tupleLength :: InsCmp CompileState m => Value -> m Int
tupleLength val = trace "tupleLength" $ do
    Tuple xs <- assertBaseType isTuple (valType val)
    return (length xs)


tupleSet :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tupleSet tup i val = trace "tupleSet" $ do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (fromIntegral i < length ts) "invalid tuple index"
    ptr <- tupleIdx i tup
    valStore ptr val


tupleMember :: InsCmp CompileState m => String -> Value -> m Value
tupleMember sym tup = trace "tupleMember" $ do
    let typ = valType tup
    assert (isTypedef typ) "Cannot have member of raw tuple"
    assertBaseType isTuple typ

    ObjMember i <- look (Sym sym) (KeyMember typ)
    tupleIdx i tup


tupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
tupleIdx i tup = trace "tupleIndex" $ do
    Tuple ts <- baseTypeOf (valType tup)
    case tup of
        Ptr _ loc -> Ptr (ts !! i) <$> gep loc [int32 0, int32 $ fromIntegral i]
        Val _ op  -> Val (ts !! i) <$> extractValue op [fromIntegral i]


tupleConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
tupleConstruct tupTyp vals = trace "tupleConstruct" $ do
    Tuple ts <- assertBaseType isTuple tupTyp
    loc <- valLocal tupTyp

    case vals of
        []    -> valStore loc =<< valZero tupTyp

        [val] -> do
            baseVal <- baseTypeOf (valType val)
            baseTup <- baseTypeOf tupTyp
            if baseVal == baseTup
            then do -- contructing from another tuple
                forM_ (zip ts [0..]) $ \(t, i) -> tupleSet loc i =<< tupleIdx i val
            else do
                assert (length ts == 1) "Invalid tuple constructor"
                void $ tupleSet loc 0 val

        vals -> do
            assert (length vals == length ts) "Invalid number of args"
            forM_ (zip ts [0..]) $ \(t, i) -> tupleSet loc i (vals !! i)

    return loc

