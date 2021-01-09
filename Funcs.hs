{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Funcs where

import CompileState
import Monad

import qualified LLVM.AST.Constant          as C
import qualified LLVM.Internal.FFI.DataLayout   as FFI
import LLVM.Context
import LLVM.AST                   hiding (function, Module)
import LLVM.AST.IntegerPredicate
import LLVM.AST.Type              hiding (void, double)
import LLVM.AST.Constant
import LLVM.Internal.Type
import LLVM.Internal.EncodeAST
import LLVM.Internal.Coding           hiding (alloca)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

cons = ConstantOperand

int16 = ConstantOperand . Int 16

fnOp :: Name -> [Type] -> Type -> Bool -> Operand
fnOp name argTypes retty isVarg =
    cons $ GlobalReference (ptr $ FunctionType retty argTypes isVarg) name


printf :: InsCmp CompileState m => String -> [Operand] -> m Operand
printf fmt args = do
    op <- ensureExtern (mkName "printf") [ptr i8] i32 True
    str <- globalStringPtr fmt =<< fresh
    call op $ map (\a -> (a, [])) (cons str:args)
