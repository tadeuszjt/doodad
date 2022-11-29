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
import Symbol


tupleTypeDef :: InsCmp CompileState m => Symbol -> S.AnnoType -> m ()
tupleTypeDef symbol (S.AnnoType typ) = trace "tupleTypeDef" $ do
    base@(Tuple ts) <- assertBaseType isTuple typ
    name <- addTypeDef symbol =<< opTypeOf base

    let typdef = Typedef symbol

    define symbol (KeyFunc [] typdef) ObjConstructor
    define symbol (KeyFunc [typ] typdef) ObjConstructor
    define symbol (KeyFunc [typdef] typdef) ObjConstructor
    define symbol KeyType $ ObType typ (Just name)

    when (length ts > 0) $
        define symbol (KeyFunc ts typdef) ObjConstructor



tupleTypeDef symbol (S.AnnoTuple xs) = trace "tupleTypeDef" $ do
    let typdef = Typedef symbol
    let tupTyp = Tuple (map snd xs)
    name <- addTypeDef symbol =<< opTypeOf tupTyp

    define symbol (KeyFunc [] typdef) ObjConstructor
    define symbol (KeyFunc [tupTyp] typdef) ObjConstructor
    define symbol (KeyFunc [typdef] typdef) ObjConstructor
    define symbol KeyType $ ObType tupTyp (Just name)

    when (length xs > 0) $ do
        define symbol (KeyFunc (map snd xs) typdef) ObjConstructor

    forM_ (zip xs [0..]) $ \((s, t), i) -> do
        define (Sym s) (KeyField typdef) (ObjField i)


tupleLength :: InsCmp CompileState m => Value -> m Int
tupleLength val = trace "tupleLength" $ do
    Tuple ts <- assertBaseType isTuple (valType val)
    return (length ts)


valTupleField :: InsCmp CompileState m => String -> Value -> m Value
valTupleField sym tup = trace "tupleField" $ do
    let typ = valType tup
    assert (isTypedef typ) "Cannot have member of raw tuple"
    assertBaseType isTuple typ
    ObjField i <- look (Sym sym) (KeyField typ)
    valTupleIdx i tup


ptrTupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
ptrTupleIdx i tup = withErrorPrefix "tuple idx: " $ do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (isPtr tup)               "tuple isnt pointer"
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Ptr (ts !! i) <$> gep (valLoc tup) [int32 0, int32 $ fromIntegral i]


valTupleIdx :: InsCmp CompileState m => Int -> Value -> m Value
valTupleIdx i tup = do
    Tuple ts <- assertBaseType isTuple (valType tup)
    assert (i >= 0 && i < length ts) "tuple index out of range"
    Val (ts !! i) <$> case tup of
        Val _ op  -> extractValue op [fromIntegral $ i]
        Ptr _ loc -> (flip load) 0 =<< gep loc [int32 0, int32 $ fromIntegral i]
