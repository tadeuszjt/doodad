{-# LANGUAGE FlexibleContexts #-}
module Map where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST.Instruction

import qualified AST
import Monad
import Value
import State
import Funcs
import Type 
import Tuple
import Typeof
import Trace
import Error
import Table
import Builtin


ptrMapKeys :: InsCmp CompileState m => Value -> m Value
ptrMapKeys map = do 
    Map tk tv <- assertBaseType isMap (valType map)
    assert (isPtr map) "val isnt pointer"
    Ptr (Table [tk]) <$> gep (valLoc map) [int32 0, int32 0]
