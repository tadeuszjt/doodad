{-# LANGUAGE FlexibleContexts #-}
module Construct where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant       
import qualified LLVM.AST.Constant as C
import LLVM.AST.Name

import Type
import State
import Monad
import Tuple
import Value
import Typeof
import Trace
import Table
import Funcs
import Error
import qualified AST as S
import Symbol
import Builtin
import ADT

valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = valZero typ
valConstruct typ (a:b:xs) = tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = do
    val <- valLoad val'
    base <- baseTypeOf typ

    case base of
        _ | isIntegral base || isFloat base -> valConvertNumber typ val
        _ | isADT base                      -> do
            mal <- valMalloc (valType val) (valI64 1)
            valStore mal =<< valCopy val
            adtConstruct typ mal

        _ -> do
            assert (typ  == valType val) "mismatched types"
            Val typ . valOp <$> valLoad val
