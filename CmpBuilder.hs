{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module CmpBuilder where

import           Control.Monad
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void, state)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Char
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST                   hiding (function)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (void)
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C

import qualified SymTab
import           Cmp

mkBSS = BSS.toShort . BS.pack




for :: Operand -> (Operand -> InstrCmp t ()) -> InstrCmp t ()
for num f = do
	forCond <- freshName (mkBSS "for.cond")
	forBody <- freshName (mkBSS "for.body")
	forExit <- freshName (mkBSS "for.exit")

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



