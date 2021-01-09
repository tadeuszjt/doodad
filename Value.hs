{-# LANGUAGE FlexibleContexts #-}
module Value where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS

import           Data.Maybe
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Fail         hiding (fail)
import           Control.Monad.Identity     
import qualified Data.Set as Set
import qualified Data.Map as Map

import           LLVM.AST                   hiding (Type, function, Module)
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (Type, void, double)
import           LLVM.Internal.Type
import           LLVM.Internal.EncodeAST
import           LLVM.Internal.Coding           hiding (alloca)
import           Foreign.Ptr
import qualified LLVM.Internal.FFI.DataLayout   as FFI
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import Monad
import CompileState
import qualified AST as S
import qualified Type as T


valInt :: T.Type -> Integer -> Value
valInt T.I8 n  = Val T.I8  (int8 n)
valInt T.I32 n = Val T.I32 (int32 n)
valInt T.I64 n = Val T.I64 (int64 n)


valBool :: Bool -> Value
valBool b = Val T.Bool (if b then bit 1 else bit 0)


valLoad :: InsCmp CompileState m => Value -> m Value
valLoad (Val typ op)    = return (Val typ op)
valLoad (Ptr T.I64 loc) = fail (show loc)
