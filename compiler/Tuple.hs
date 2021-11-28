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

valIsTuple :: InsCmp CompileState m => Value -> m Bool
valIsTuple (Exp (S.Tuple _ _)) = return True
valIsTuple (Exp _)             = return False
valIsTuple tup                 = isTuple <$> baseTypeOf (valType tup)


tupleLength :: InsCmp CompileState m => Value -> m Int
tupleLength (Exp (S.Tuple _ vs)) = return (length vs)
tupleLength val                  = do
    Tuple xs <- assertBaseType isTuple (valType val)
    return (length xs)


tupleSet :: InsCmp CompileState m => Value -> Int -> Value -> m ()
tupleSet tup i val = do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (fromIntegral i < length ts) "invalid tuple index"
    ptr <- tupleIdx i tup
    valStore ptr val


tupleMember :: InsCmp CompileState m => String -> Value -> m Value
tupleMember sym tup = do
    Tuple xs <- assertBaseType isTuple (valType tup)
    let is = [ i | ((s, t), i) <- zip xs [0..], s == sym ]

    assert (length is == 1) $ "tuple has ambigous member: " ++ (show $ fst $ head xs)
    tupleIdx (head is) tup


tupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
tupleIdx i tup = do
    b <- valIsTuple tup
    assert b "isn't a tuple"
    case tup of
        Exp (S.Tuple _ es) -> return $ Exp (es !! i)
        Ptr _ loc          -> do
            Tuple xs <- baseTypeOf (valType tup)
            Ptr (snd $ xs !! i) <$> gep loc [int32 0, int32 $ fromIntegral i]
        Val _ op           -> do
            Tuple xs <- baseTypeOf (valType tup)
            Val (snd $ xs !! i) <$> extractValue op [fromIntegral i]


tupleConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
tupleConstruct tupTyp vals = do
    Tuple xs <- assertBaseType isTuple tupTyp
    tup <- valLocal tupTyp
    case vals of
        []    -> return ()
        [val] -> do
            pureVal <- pureTypeOf (valType val)
            pureTup <- pureTypeOf tupTyp
            if pureVal == pureTup
            then do -- contructing from another tuple
                forM_ (zip xs [0..]) $ \((s, t), i) -> tupleSet tup i =<< tupleIdx i val
            else do
                assert (length xs == 1) "Invalid tuple constructor"
                void $ tupleSet tup 0 val
        vals -> do
            assert (length vals == length xs) "Invalid number of args"
            forM_ (zip xs [0..]) $ \((s, t), i) -> tupleSet tup i (vals !! i)
    return tup

