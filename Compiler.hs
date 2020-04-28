module Compiler where

import           Control.Monad
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST                   hiding (function)
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
import           CmpState
import           CmpBuilder


codeGen :: CmpState Operand Definition -> S.AST -> (Either CmpError ((), [Definition]), CmpState Operand Definition)
codeGen cmpState ast =
	(runState . runExceptT . getCmp) (runModuleBuilderT emptyModuleBuilder cmp) cmpState
	where
		cmp :: ModuleGen ()
		cmp = do
			function (mkName "main") [] VoidType $ \_ ->
				mapM_ cmpTopStmt ast
			return ()


cmpTopStmt :: S.Stmt -> InstrGen ()
cmpTopStmt stmt = case stmt of
	S.Print _ _      -> cmpStmt stmt
	S.CallStmt _ _ _ -> cmpStmt stmt
	S.Map _ _ _      -> cmpStmt stmt

	S.Assign pos symbol expr -> do
		l2 (checkSymbolUndefined pos symbol)
		val <- cmpExpr expr
		name <- freshName (mkBSS symbol)

		let typ = typeOf val
		let ext = globalDef name typ Nothing

		l2 (addDeclared symbol)
		l2 (addExported symbol)
		l2 (addExtern symbol ext)

		if isCons val then do
			addr <- global name typ (toCons val)
			l2 (addSymbol symbol addr)
		else do
			addr <- global name typ (initOf typ)
			l2 (addSymbol symbol addr)
			void (store addr 0 val)


	S.Func pos symbol params retty stmts -> do
		l2 (checkSymbolUndefined pos symbol)
		name <- freshName (mkBSS symbol)
		let Name nameStr = name

		let mapType = \t -> case t of
			S.I64 -> i64
			S.TBool -> i1

		let paramSymbols = map S.paramName params
		let paramTypes   = map (mapType . S.paramType) params
		let paramNames   = map (ParameterName . mkBSS) paramSymbols
		let returnType   = maybe VoidType mapType retty

		let op = cons $ C.GlobalReference (ptr $ FunctionType returnType paramTypes False) name
		let ext = funcDef name (zip paramTypes $ map mkName paramSymbols) returnType []

		l2 (addDeclared symbol)
		l2 (addExtern symbol ext)
		l2 (addSymbol symbol op)
		l2 (addExported symbol)

		lift $ function name (zip paramTypes paramNames) returnType $ \args -> (flip named) nameStr $ do
			l2 pushSymTab
			curRetType <- l2 getCurRetType
			l2 (setCurRetType returnType)

			forM (zip paramSymbols args) $ \(sym, arg) -> do
				op <- alloca (typeOf arg) Nothing 0
				store op 0 arg
				l2 (addSymbol sym op)

			mapM_ cmpStmt stmts
			l2 (setCurRetType curRetType)
			l2 popSymTab

		return ()


cmpStmt :: S.Stmt -> InstrGen ()
cmpStmt stmt = case stmt of
	S.Assign pos symbol expr -> do
		l2 (checkSymbolUndefined pos symbol)
		val <- cmpExpr expr
		op <- alloca (typeOf val) Nothing 0
		store op 0 val
		l2 (addSymbol symbol op)

	S.Set pos symbol expr -> do
		op <- lift (look pos symbol)
		store op 0 =<< cmpExpr expr

	S.Block pos stmts -> do
		l2 pushSymTab
		mapM_ cmpStmt stmts
		l2 popSymTab

	S.CallStmt pos symbol args -> do
		op <- lift (look pos symbol)
		paramTypes <- case typeOf op of
			PointerType (FunctionType _ pts _) _ -> return pts
			_                  -> l2 $ cmpErr pos (symbol ++ " isn't a function")

		vals <- mapM cmpExpr args
		unless (map typeOf vals == paramTypes) (l2 $ cmpErr pos "arg types don't match")
		void $ call op $ map (\arg -> (arg, [])) vals

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs
		sep <- globalStringPtr ", " =<< fresh

		forM_ vals $ \val -> do
			case typeOf val of
				IntegerType 1 -> do
					str <- globalStringPtr "true\0false" =<< fresh
					idx <- select val (int8 0) (int8 5)
					ptr <- gep (cons str) [idx]
					return ()
				IntegerType _ ->
					void (printf "%d" [val])
				PointerType (IntegerType 8) _ ->
					void $ printf "%s" [val]
				PointerType (ArrayType n t) _ -> do
					putchar '['
					for (int64 $ fromIntegral n) $ \i -> do
						ptr <- gep val [int64 0, i]
						elem <- load ptr 0
						printf "%d, " [elem]
						return ()
					putchar ']'
					return ()
				t ->
					error ("can't print type: " ++ show t)

			printf "%s" [cons sep]

		void (putchar '\n')

	S.Map pos symbol expr -> do
		val <- cmpExpr expr
		(elemType, len) <- case typeOf val of
			PointerType (ArrayType n t) _ -> return (t, n)
			_ -> l2 (cmpErr pos "expression isn't an array")

		fun <- lift (look pos symbol)
		argTypes <- case typeOf fun of
			PointerType (FunctionType _ at _) _ -> return at
			_ -> l2 $ cmpErr pos (symbol ++ " isn't a function")

		unless (argTypes == [elemType]) (l2 $ cmpErr pos "incorrect function type")

		for (int64 $ fromIntegral len) $ \i -> do
			ptr <- gep val [int64 0, i]
			elem <- load ptr 0
			void $ call fun [(elem, [])]

	S.Return pos expr -> do
		typ <- l2 getCurRetType
		if isNothing expr then do
			unless (typ == VoidType) (l2 $ cmpErr pos "return value required")
			retVoid
		else do
			unless (typ /= VoidType) (l2 $ cmpErr pos "cannot return value in void function")
			val <- cmpExpr (fromJust expr)
			unless (typ == typeOf val) (l2 $ cmpErr pos "wrong type")
			ret val
		void block

	S.If pos expr block els -> do
		cnd <- cmpExpr expr
		unless (typeOf cnd == i1) (l2 $ cmpErr pos "expression isn't boolean")
		true <- freshName (mkBSS "if.true")
		false <- freshName (mkBSS "if.false")
		cont <- freshName (mkBSS "if.cont")
		condBr cnd true false
		emitBlockStart true
		cmpStmt block
		br cont
		emitBlockStart false
		unless (isNothing els) $ cmpStmt (fromJust els)
		br cont
		emitBlockStart cont


cmpExpr :: S.Expr -> InstrGen Operand
cmpExpr expr = case expr of
	S.Int pos n      -> return (int64 n)
	S.Bool pos b     -> return $ bit (if b then 1 else 0)
	S.String pos str -> fmap cons (globalStringPtr str =<< fresh)

	S.Ident pos symbol -> do
		val <- lift (look pos symbol)
		case typeOf val of
			PointerType (IntegerType _) _ -> load val 0
			_ -> l2 $ cmpErr pos (symbol ++ " isn't an expression")

	S.Call pos symbol args -> do
		op <- lift (look pos symbol)
		vals <- mapM cmpExpr args

		typ <- case typeOf op of
			PointerType t@(FunctionType _ _ _) _ -> return t
			_                                    -> l2 $ cmpErr pos (symbol ++ " isn't a function")

		let FunctionType retType argTypes _ = typ
		when (retType == VoidType) (l2 $ cmpErr pos "cannot use a void function as an expression")
		unless (map typeOf vals == argTypes) (l2 $ cmpErr pos "argument types don't match")

		call op [(val, []) | val <- vals]

	S.Array pos exprs -> do
		vals <- mapM cmpExpr exprs
		let typ = typeOf (head vals)
		let num = fromIntegral (length vals)
		unless (num > 0) (l2 $ cmpErr pos "cannot deduce array type")
		unless (all (== typ) (map typeOf vals)) (l2 $ cmpErr pos "element types don't match")

		name <- fresh

		if all isCons vals then
			lift $ global name (ArrayType num typ) (C.Array typ $ map toCons vals)
		else
			l2 (cmpErr pos "todo")

	S.Prefix pos op expr -> do
		val <- cmpExpr expr
		case typeOf val of
			IntegerType 1 -> l2 (cmpErr pos "unsupported prefix")
			IntegerType n -> case op of
				S.Plus  -> return val
				S.Minus -> sub (cons $ C.Int n 0) val
				_       -> l2 (cmpErr pos "unsupported prefix")

	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		case (typeOf val1, typeOf val2) of
			(IntegerType 1, IntegerType 1) -> do
				case op of
					S.EqEq   -> icmp EQ val1 val2
					S.AndAnd -> and val1 val2
					S.OrOr   -> or val1 val2
					_        -> l2 (cmpErr pos "unsupported prefix")

			(IntegerType aBits, IntegerType bBits) -> do
				unless (aBits == bBits) (l2 $ cmpErr pos "integer types don't match")
				case op of
					S.Plus   -> add val1 val2
					S.Minus  -> sub val1 val2
					S.Times  -> mul val1 val2
					S.Divide -> sdiv val1 val2
					S.Mod    -> srem val1 val2
					S.LT     -> icmp SLT val1 val2 >>= \i -> trunc i i1
					S.GT     -> icmp SGT val1 val2 >>= \i -> trunc i i1
					S.LTEq   -> icmp SLE val1 val2 >>= \i -> trunc i i1
					S.GTEq   -> icmp SGT val1 val2 >>= \i -> trunc i i1
					S.EqEq   -> icmp EQ val1 val2 >>= \i -> trunc i i1
					_        -> l2 (cmpErr pos "unsupported prefix")
