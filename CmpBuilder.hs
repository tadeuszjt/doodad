{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CmpBuilder where

import           Data.Char
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST 
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (void)
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Monad
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C

import           Cmp


for :: Operand -> (Operand -> InstrCmp t ()) -> InstrCmp t ()
for num f = do
	forCond <- freshName "for.cond"
	forBody <- freshName "for.body"
	forExit <- freshName "for.exit"

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
	return ()


putchar :: Char -> InstrCmp t Operand
putchar ch = do
	op <- ensureExtern "putchar" [i32] i32 False
	let c8 = fromIntegral (ord ch)
	call op [(int32 c8, [])]


printf :: String -> [Operand] -> InstrCmp t Operand
printf fmt args = do
	op <- ensureExtern "printf" [ptr i8] i32 True
	str <- globalStringPtr fmt =<< fresh
	call op $ map (\a -> (a, [])) (cons str:args)


puts :: Operand -> InstrCmp t Operand
puts str = do
	op <- ensureExtern "puts" [ptr i8] i32 False
	call op [(str, [])]


initOf :: Type -> C.Constant
initOf typ = case typ of
	IntegerType nbits -> C.Int nbits 0
	ArrayType n t     -> C.Array t (replicate (fromIntegral n) (initOf t))
	_                 -> error (show typ)


isCons :: Operand -> Bool
isCons (ConstantOperand _) = True
isCons _                   = False


toCons :: Operand -> C.Constant
toCons (ConstantOperand c) = c


cons :: C.Constant -> Operand
cons = ConstantOperand


globalDef :: Name -> Type -> Maybe C.Constant -> Definition
globalDef nm ty init = GlobalDefinition globalVariableDefaults
	{ G.name        = nm
	, G.type'       = ty
	, G.initializer = init
	}


funcDef :: Name -> [(Type, Name)] -> Type -> [BasicBlock] -> Definition
funcDef nm params retty blocks = GlobalDefinition functionDefaults
	{ G.name        = nm
	, G.parameters  = ([Parameter t n [] | (t, n) <- params], False)
	, G.returnType  = retty
	, G.basicBlocks = blocks
	}



