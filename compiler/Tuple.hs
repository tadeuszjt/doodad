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

tupleTypeDef :: InsCmp CompileState m => S.Symbol -> Type -> m ()
tupleTypeDef sym typ = trace "tupleTypeDef" $ do
    Tuple xs <- assertBaseType isTuple typ 
    let typdef = Typedef sym

    addObjWithCheck sym (KeyFunc []) (ObjConstructor typdef)
    addObjWithCheck sym (KeyFunc [typ]) (ObjConstructor typdef)
    addObjWithCheck sym (KeyFunc [typdef]) (ObjConstructor typdef)
    addObjWithCheck sym KeyType $ ObType typ Nothing

    let ts = map snd xs
    when (length ts > 0) $
        addObjWithCheck sym (KeyFunc ts) (ObjConstructor typdef)


tupleLength :: InsCmp CompileState m => Value -> m Int
tupleLength (Exp (S.Tuple _ es)) = trace "tupleLength" $ return (length es)
tupleLength val                  = trace "tupleLength" $ do
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
    Tuple xs <- assertBaseType isTuple (valType tup)
    let is = [ i | ((s, t), i) <- zip xs [0..], s == sym ]

    assert (length is == 1) $ "tuple has ambigous member: " ++ (show $ fst $ head xs)
    tupleIdx (head is) tup


tupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
tupleIdx i (Exp (S.Tuple _ es)) = trace "tupleIdx"   $ return $ Exp (es !! i)
tupleIdx i tup                  = trace "tupleIndex" $ do
    Tuple xs <- baseTypeOf (valType tup)
    case tup of
        Ptr _ loc -> Ptr (snd $ xs !! i) <$> gep loc [int32 0, int32 $ fromIntegral i]
        Val _ op  -> Val (snd $ xs !! i) <$> extractValue op [fromIntegral i]


tupleConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
tupleConstruct tupTyp vals = trace "tupleConstruct" $ do
    Tuple xs <- assertBaseType isTuple tupTyp
    loc <- valLocal tupTyp

    case vals of
        []    -> valStore loc =<< valZero tupTyp

        [val] -> do
            baseVal <- baseTypeOf (valType val)
            baseTup <- baseTypeOf tupTyp
            if baseVal == baseTup
            then do -- contructing from another tuple
                forM_ (zip xs [0..]) $ \((s, t), i) -> tupleSet loc i =<< tupleIdx i val
            else do
                assert (length xs == 1) "Invalid tuple constructor"
                void $ tupleSet loc 0 val

        vals -> do
            assert (length vals == length xs) "Invalid number of args"
            forM_ (zip xs [0..]) $ \((s, t), i) -> tupleSet loc i (vals !! i)

    return loc

