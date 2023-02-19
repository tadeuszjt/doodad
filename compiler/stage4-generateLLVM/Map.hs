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


mapKeys :: InsCmp CompileState m => Pointer -> m Pointer
mapKeys map = do 
    Map tk tv <- baseTypeOf map
    tableRow 0 $ Pointer (Table [tk, tv]) (loc map)


mapValues :: InsCmp CompileState m => Pointer -> m Pointer
mapValues map = do 
    Map tk tv <- baseTypeOf map
    tableRow 1 $ Pointer (Table [tk, tv]) (loc map)


mapLen :: InsCmp CompileState m => Pointer -> m Pointer 
mapLen map = do 
    Map tk tv <- baseTypeOf map
    tableLen $ Pointer (Table [tk, tv]) (loc map)

    



    
