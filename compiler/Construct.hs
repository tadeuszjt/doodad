{-# LANGUAGE FlexibleContexts #-}
module Construct where

import Debug.Trace

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       

import Type
import State
import Monad
import Tuple
import Value
import ADT
import Typeof

valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []           = trace ("valConstruct " ++ show typ ++ "()") $ zeroOf typ
valConstruct typ [val]        = trace "valConstruct" $ do
    val' <- valLoad =<< valResolveContextual val

    if valType val' == typ
    then return val'
    else do
        base <- baseTypeOf typ
        case base of
            Table [Char] -> do
                ObjFunc retty op <- look "string" (KeyFunc [valType val'])
                Val retty <$> call op [(valOp val', [])]

            I32 -> case val' of
                Val I64 op -> Val typ <$> trunc op LL.i32
                Val I8 op  -> Val typ <$> sext op LL.i32

            I64 -> case val' of
                Val Char op -> Val typ <$> sext op LL.i64

            Char -> case val' of
                Val I64 op -> Val typ <$> trunc op LL.i8
                Val I32 op -> Val typ <$> trunc op LL.i8
                _          -> error (show val')

            ADT _       -> adtConstruct typ val'

            _           -> do
                assert (base == valType val' || typ == valType val') "Types do not match"
                Val typ <$> valOp <$> valLoad val'
