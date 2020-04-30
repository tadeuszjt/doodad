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


type State = CmpState ValType
type Instr        = InstrCmp ValType
type Module       = ModuleCmp ValType


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
	S.Print _ _      -> cmpStmt stmt
	S.CallStmt _ _ _ -> cmpStmt stmt
	S.Map _ _ _      -> cmpStmt stmt

	S.Assign pos symbol expr -> do
		checkSymbolUndefined pos symbol
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

		(addDeclared symbol)


--	S.Func pos symbol [] retty stmts -> do
--		(checkSymbolUndefined pos symbol)
--		name <- freshName (mkBSS symbol)
--		let Name nameStr = name
--
--		returnType <- fmap fromValType $ case retty of
--			Nothing     -> return Void
--			Just S.I64  -> return I64
--			Just S.TBool -> return Bool
--			_ -> cmpErr pos "invalid return type"
--
--		let typ = FunctionType returnType [] False
--		let op = cons $ C.GlobalReference (ptr typ) name
--		let ext = funcDef name [] returnType []
--
--		(addDeclared symbol)
--		(addExported symbol)
--		(addExtern symbol ext)
--		$ addSymbol symbol (Func, op)
--
----		lift $ function name [] returnType $ \args -> (flip named) nameStr $ do
----			pushSymTab
----			curRetType <- getState
----
------			forM (zip paramSymbols args) $ \(sym, arg) -> do
------				op <- alloca (typeOf arg) Nothing 0
------				store op 0 arg
------				$ addSymbol sym (I64, op)
----
----			mapM_ cmpStmt stmts
----			(setState curRetType)
----			popSymTab
--
--		return ()


cmpStmt :: S.Stmt -> Instr ()
cmpStmt stmt = case stmt of
	S.Assign pos symbol expr -> do
		(checkSymbolUndefined pos symbol)

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
		(styp, sop) <- look pos symbol
		(etyp, eop) <- cmpExpr expr
		unless (styp == etyp) (cmpErr pos "types don't match")

		case styp of
			Bool -> store sop 0 eop
			I64  -> store sop 0 eop

		return ()
			
	S.Block pos stmts -> do
		pushSymTab
		mapM_ cmpStmt stmts
		popSymTab

--	S.CallStmt pos symbol args -> do
--		(typ, op) <- look pos symbol
--		paramTypes <- case typeOf op of
--			PointerType (FunctionType _ pts _) _ -> return pts
--			_ -> $ cmpErr pos (symbol ++ " isn't a function")
--
--		vals <- mapM cmpExpr args
--		unless (map typeOf vals == paramTypes) ($ cmpErr pos "arg types don't match")
--		void $ call op $ map (\arg -> (arg, [])) vals

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs
		sep <- globalStringPtr ", " =<< fresh

		forM_ vals $ \(typ, op) -> do
			case typ of
				Bool -> do
					str <- globalStringPtr "true\0false" =<< fresh
					idx <- select op (int64 0) (int64 5)
					ptr <- gep (cons str) [idx]
					return ()

				I64 -> void (printf "%ld" [op])

				t -> error ("can't print type: " ++ show t)

			printf "%s" [cons sep]

		void (putchar '\n')

--	S.Return pos expr -> do
--		curRetty <- getState
--
--		if isNothing expr then do
--			unless (curRetty == Void) (cmpErr pos "return value required")
--			retVoid
--		else do
--			unless (curRetty /= Void) (cmpErr pos "cannot return value in void function")
--			(typ, op) <- cmpExpr (fromJust expr)
--			unless (typ == curRetty) (cmpErr pos "wrong type")
--			ret op
--
--		void block

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
	S.Int pos i      -> return (I64, int64 i)
	S.Bool pos b     -> return (Bool, bit $ if b then 1 else 0)

	S.Ident pos symbol -> do
		(typ, op) <- look pos symbol

		op' <- case typ of
			I64  -> load op 0
			Bool -> load op 0
			_   -> cmpErr pos (symbol ++ " isn't an expression")

		return (typ, op')

--	S.Call pos symbol args -> do
--		(typ, op) <- look pos symbol
--		case typ of
--			_ -> cmpErr pos (symbol ++ " isn't a function")
--
--		vals <- mapM cmpExpr args
--
--		let FunctionType retType argTypes _ = typ
--		when (retType == VoidType) ($ cmpErr pos "cannot use a void function as an expression")
--		unless (map typeOf vals == argTypes) ($ cmpErr pos "argument types don't match")
--
--		call op [(val, []) | val <- vals]

	S.Prefix pos operator expr -> do
		(typ, op) <- cmpExpr expr
		case typ of
			I64 -> case operator of
				S.Plus  -> return (typ, op)
				S.Minus -> return . (I64,) =<< sub (int64 0) op
				_       -> (cmpErr pos "unsupported prefix")
			_ -> (cmpErr pos "unsupported prefix")

	S.Infix pos op expr1 expr2 -> do
		(typ1, val1) <- cmpExpr expr1
		(typ2, val2) <- cmpExpr expr2

		case (typeOf val1, typeOf val2) of
			(IntegerType 1, IntegerType 1) -> do
				case op of
					S.EqEq   -> icmp EQ val1 val2 >>= return . (Bool,)
					S.AndAnd -> and val1 val2 >>= return . (Bool,)
					S.OrOr   -> or val1 val2 >>= return . (Bool,)
					_        -> (cmpErr pos "unsupported prefix")

			(IntegerType 64, IntegerType 64) -> do
				case op of
					S.Plus   -> add val1 val2 >>= \o -> return (I64, o)
					S.Minus  -> sub val1 val2 >>= \o -> return (I64, o)
					S.Times  -> mul val1 val2 >>= \o -> return (I64, o)
					S.Divide -> sdiv val1 val2 >>= \o -> return (I64, o)
					S.Mod    -> srem val1 val2 >>= \o -> return (I64, o)
					S.LT     -> icmp SLT val1 val2 >>= \i -> trunc i i1 >>= \o -> return (Bool, o)
					S.GT     -> icmp SGT val1 val2 >>= \i -> trunc i i1 >>= \o -> return (Bool, o)
					S.LTEq   -> icmp SLE val1 val2 >>= \i -> trunc i i1 >>= \o -> return (Bool, o)
					S.GTEq   -> icmp SGT val1 val2 >>= \i -> trunc i i1 >>= \o -> return (Bool, o)
					S.EqEq   -> icmp EQ val1 val2 >>= \i -> trunc i i1 >>= \o -> return (Bool, o)
					_        -> (cmpErr pos "unsupported prefix")
			(ta, tb) -> error $ show (ta, tb)
