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

valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = trace "valConstruct" $ valZero typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = trace "valConstruct" $ do
    val <- valLoad val'
    base <- baseTypeOf typ

    case base of
        t | isIntegral t || isFloat t -> valConvertNumber typ val

        _ -> do
            assert (typ  == valType val) "mismatched types"
            Val typ . valOp <$> valLoad val
