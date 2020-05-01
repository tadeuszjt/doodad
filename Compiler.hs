{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import           Control.Monad
import           Control.Monad.Except       hiding (void)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST                   hiding (function, Module)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (void)
import           LLVM.AST.Typed
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified Lexer                      as L
import qualified AST                        as S
import           CmpBuilder
import           Cmp


data ValType
	= Void
	| I64
	| Bool
	| Func
	deriving (Show, Eq)


type State  = CmpState ValType
type Instr  = InstrCmp ValType
type Module = ModuleCmp ValType


compile :: State -> S.AST -> Either CmpError ([Definition], State)
compile state ast =
	fmap (\((_, defs), st) -> (defs, st)) (runModuleCmp emptyModuleBuilder state cmp)
	where
		cmp :: Module ()
		cmp =
			void $ function "main" [] VoidType $ \_ ->
				getInstrCmp (mapM_ cmpTopStmt ast)


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt = case stmt of
	S.Set _ _ _      -> cmpStmt stmt
	S.Print _ _      -> cmpStmt stmt
	S.CallStmt _ _ _ -> cmpStmt stmt

	S.Assign pos symbol expr -> do
		checkUndefined pos symbol
		name <- freshName (mkBSS symbol)
		(typ, op) <- cmpExpr expr

		case typ of
			Bool -> do
				loc <- global name i1 (toCons $ bit 0)
				store loc 0 op
				addExtern symbol (globalDef name i1 Nothing)
				addSymbol symbol (Bool, loc)

			I64 -> do
				loc <- global name i64 (toCons $ int64 0)
				store loc 0 op
				addExtern symbol (globalDef name i64 Nothing)
				addSymbol symbol (I64, loc)

		addDeclared symbol


cmpStmt :: S.Stmt -> Instr ()
cmpStmt stmt = case stmt of
	S.Assign pos symbol expr -> do
		checkUndefined pos symbol
		(typ, op) <- cmpExpr expr

		case typ of
			Bool -> do
				loc <- alloca i1 Nothing 0
				store loc 0 op
				addSymbol symbol (Bool, op)

			I64 -> do
				loc <- alloca i64 Nothing 0
				store loc 0 op
				addSymbol symbol (Bool, op)


	S.Set pos symbol expr -> do
		(symType, loc) <- look pos symbol
		(exprType, op) <- cmpExpr expr
		unless (symType == exprType) (cmpErr pos "types don't match")

		void $ case symType of
			Bool -> store loc 0 op
			I64  -> store loc 0 op

			
	S.Block pos stmts -> do
		pushSymTab
		mapM_ cmpStmt stmts
		popSymTab


	S.Print pos exprs -> do
		let print = \(typ, op) -> case typ of
			I64  -> void (printf "%ld" [op])

			Bool -> do
				str <- globalStringPtr "true\0false" =<< fresh
				idx <- select op (int64 0) (int64 5)
				ptr <- gep (cons str) [idx]
				void (puts ptr)

			t -> cmpErr pos ("can't print type: " ++ show typ)

		let prints = \vals -> case vals of
			[]     -> return ()
			[val]  -> print val
			(v:vs) -> print v >> printf ", " [] >> prints vs
			
		vals <- mapM cmpExpr exprs
		prints vals
		void (putchar '\n')


	S.If pos expr block els -> do
		(typ, cnd) <- cmpExpr expr
		unless (typ == Bool) (cmpErr pos "expression isn't boolean")

		true  <- freshName (mkBSS "if.true")
		false <- freshName (mkBSS "if.false")
		exit  <- freshUnName

		condBr cnd true false

		emitBlockStart true
		cmpStmt block
		br exit

		emitBlockStart false
		when (isJust els) $ cmpStmt (fromJust els)
		br exit

		emitBlockStart exit


cmpExpr :: S.Expr -> Instr (ValType, Operand)
cmpExpr expr = case expr of
	S.Int pos i  -> return (I64, int64 i)
	S.Bool pos b -> return (Bool, bit $ if b then 1 else 0)

	S.Ident pos symbol -> do
		(typ, op) <- look pos symbol

		op' <- case typ of
			I64  -> load op 0
			Bool -> load op 0
			_ -> cmpErr pos (symbol ++ " isn't an expression")

		return (typ, op')


	S.Prefix pos operator expr -> do
		(typ, op) <- cmpExpr expr

		case typ of
			I64 -> case operator of
				S.Plus  -> return (I64, op)
				S.Minus -> fmap (I64,) $ sub (int64 0) op
				_       -> cmpErr pos "unsupported prefix"
			_ -> cmpErr pos "unsupported prefix"

	S.Infix pos operator expr1 expr2 -> do
		(typ1, val1) <- cmpExpr expr1
		(typ2, val2) <- cmpExpr expr2

		case (typ1, typ2) of
			(Bool, Bool) ->
				case operator of
					S.EqEq   -> fmap (Bool,) (icmp EQ val1 val2)
					S.AndAnd -> fmap (Bool,) (and val1 val2)
					S.OrOr   -> fmap (Bool,) (or val1 val2)
					_        -> cmpErr pos "unsupported infix"

			(I64, I64) ->
				case operator of
					S.Plus   -> fmap (I64,) (add val1 val2)
					S.Minus  -> fmap (I64,) (sub val1 val2)
					S.Times  -> fmap (I64,) (mul val1 val2)
					S.Divide -> fmap (I64,) (sdiv val1 val2)
					S.Mod    -> fmap (I64,) (srem val1 val2)
					S.LT     -> fmap (Bool,) $ (flip trunc) i1 =<< icmp SLT val1 val2 
					S.GT     -> fmap (Bool,) $ (flip trunc) i1 =<< icmp SGT val1 val2
					S.LTEq   -> fmap (Bool,) $ (flip trunc) i1 =<< icmp SLE val1 val2
					S.GTEq   -> fmap (Bool,) $ (flip trunc) i1 =<< icmp SGT val1 val2
					S.EqEq   -> fmap (Bool,) $ (flip trunc) i1 =<< icmp EQ val1 val2
					_        -> cmpErr pos "unsupported infix"
			(ta, tb) -> error $ show (ta, tb)
