{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Funcs where

import CompileState
import Monad

import Control.Monad

import LLVM.AST                   hiding (function, Module)
import LLVM.AST.IntegerPredicate
import LLVM.AST.Type              hiding (void, double)
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

cons = ConstantOperand


toCons (ConstantOperand c) = c


int16 = ConstantOperand . Int 16


fnOp :: Name -> [Type] -> Type -> Bool -> Operand
fnOp name argTypes retty isVarg =
    cons $ GlobalReference (ptr $ FunctionType retty argTypes isVarg) name


printf :: InsCmp CompileState m => String -> [Operand] -> m Operand
printf fmt args = do
    op <- ensureExtern (mkName "printf") [ptr i8] i32 True
    str <- globalStringPtr fmt =<< fresh
    call op $ map (\a -> (a, [])) (cons str:args)


putchar :: InsCmp CompileState m => Operand -> m Operand
putchar ch = do
    op <- ensureExtern (mkName "putchar") [i32] VoidType False
    call op [(ch, [])]


memset :: InsCmp CompileState m => Operand -> Operand -> Operand -> m Operand 
memset p v size = do
    op <- ensureExtern "memset" [ptr i8, i64, i64] (ptr i8) False
    d <- bitcast p (ptr i8)
    call op [(d, []), (v, []), (size, [])]


malloc :: InsCmp CompileState m => Operand -> m Operand
malloc size = do
    op <- ensureExtern "GC_malloc" [i64] (ptr i8) False
    call op [(size, [])]


memcpy :: InsCmp CompileState m => Operand -> Operand -> Operand -> m Operand
memcpy dest src size = do
    op <- ensureExtern "memcpy" [ptr i8, ptr i8, i64] (ptr i8) False
    call op [(dest, []), (src, []), (size, [])]


func :: Monad m => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> InstrCmpT s m ()) -> ModuleCmpT s m Operand
func name argtys retty f = do
    let tys = map fst argtys
    (paramNames, basicBlocks) <- runInstrCmpT emptyIRBuilder $ do
        paramNames <- forM argtys $ \(_, paramName) -> case paramName of
            NoParameterName -> fresh
            ParameterName p -> fresh `named` p
        f (zipWith LocalReference tys paramNames)
        return paramNames

    let def = GlobalDefinition functionDefaults {
        name        = name,
        parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False),
        returnType  = retty,
        basicBlocks = basicBlocks
        }
    emitDefn def
    return (fnOp name tys retty False) 


for :: InsCmp s m => Operand -> (Operand -> m ()) -> m ()
for num f = do
    forCond <- freshName "for_cond"
    forBody <- freshName "for_body"
    forExit <- freshName "for_exit"

    i <- alloca i64 Nothing 0
    store i 0 (int64 0)
    br forCond

    emitBlockStart forCond
    li <- load i 0
    cnd <- icmp SLT li num 
    condBr cnd forBody forExit

    emitBlockStart forBody
    store i 0 =<< add li (int64 1)
    f li
    br forCond

    emitBlockStart forExit


if_ :: InsCmp s m => Operand -> m () -> m () -> m ()
if_ cnd trueIns falseIns = do
    true  <- freshName "if_true"
    false <- freshName "if_false"
    exit  <- freshName "if_exit"

    condBr cnd true false
    emitBlockStart true
    trueIns
    br exit

    emitBlockStart false
    falseIns
    br exit

    emitBlockStart exit

