{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Control.Monad
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import Control.Monad.Fail
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Maybe

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab
import CmpState

import LLVM.AST
import LLVM.AST.Type hiding (void)
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.IntegerPredicate
import LLVM.AST.Linkage
import LLVM.AST.CallingConvention
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C


codeGen :: CmpState -> S.AST -> (Either CmpError (), CmpState)
codeGen cmpState ast =
	runState (runExceptT $ getCmp cmp) cmpState
	where
		cmp = do
			addBlock (mkName "entry") 
			mapM_ cmpTopStmt ast


cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Set pos name expr -> cmpStmt stmt
	S.Print pos exprs -> cmpStmt stmt
	S.Block pos stmts -> cmpStmt stmt
	S.CallStmt pos name args -> cmpStmt stmt
	S.If pos expr block -> cmpStmt stmt

	S.Assign pos name expr -> do
		checkSymbolUndefined pos name

		val <- cmpExpr expr
		let typ = typeOf val

		unName <- uniqueName name
		let op = global (ptr typ) unName
		let def = globalVar typ unName False

		addSymbol name op $ Just (unName, def Nothing)
		addDeclared unName
		addExported unName
		
		if isCons val then
			addDef $ def $ Just (toCons val)
		else do
			addDef $ def $ Just (initOf typ)
			store op val

	S.Func pos name retType block -> do
		checkSymbolUndefined pos name
		unName <- uniqueName name

		let fnRetType = case retType of
			Nothing    -> VoidType
			Just S.I64 -> i64

		let fnType  = FunctionType fnRetType [] False
		let op      = global (ptr fnType) unName
		let ext     = funcDef unName fnRetType [] False []

		addSymbol name op $ Just (unName, ext)
		addDeclared unName
		addExported unName

		curTyp <- gets curRetType
		modify $ \s -> s { curRetType = fnRetType }
		pushBlocks
		addBlock (mkName "entry")
		cmpStmt block
		blocks <- popBlocks
		modify $ \s -> s { curRetType = curTyp }

		addDef $ funcDef unName fnRetType [] False blocks


cmpStmt :: S.Stmt -> Cmp ()
cmpStmt stmt = case stmt of
	S.Assign pos name expr -> do
		checkSymbolUndefined pos name
		unName <- uniqueName name

		val <- cmpExpr expr
		let typ = typeOf val
		let op = local (ptr typ) unName

		addSymbol name op Nothing
		alloca typ unName
		store op val

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs

		fmtArgs <- forM vals $ \val -> case typeOf val of
			IntegerType 64 -> return ("%d", val)
			IntegerType 1  -> do
				valI8 <- zext val i8
				boolStr <- string "false\x00true"
				boolIdx <- mul valI8 $ cons (C.Int 8 6)
				boolPtr <- subscript boolStr boolIdx
				return ("%s", boolPtr)
			t              -> error ("can't print type: " ++ show t)

		let fmts = map fst fmtArgs
		let args = map snd fmtArgs
		str <- string (intercalate ", " fmts ++ "\n")
		fmt <- subscript str $ cons (C.Int 64 0)

		let name = mkName "printf"
		let typ  = FunctionType i32 [ptr i8] False
		let op   = global (ptr typ) name
		let ext  = funcDef name i32 [Parameter (ptr i8) (mkName "fmt") []] False []

		decls <- gets declared
		unless (name `elem` decls) $ addDeclared name >> addDef ext
		void $ call op (fmt:args)

	S.Set pos name expr -> do
		op <- lookupSymbol pos name
		store op =<< cmpExpr expr
	
	S.Block pos block -> do
		pushSymTab
		mapM_ cmpStmt block
		popSymTab

	S.CallStmt pos name args -> do
		op <- lookupSymbol pos name
		let typ = typeOf op
		case typ of
			FunctionType _ _ _ -> return ()
			_                  -> cmpErr pos (name ++ " isn't a function")

		let FunctionType _ [] False = typ
		void $ call op [] 

	S.If pos expr block -> do
		val <- cmpExpr expr
		cnd <- icmp val (cons $ C.Int 64 0) NE 
		true <- uniqueName "true"
		next <- unique
		terminator (cndBr cnd true next)
		addBlock true
		cmpStmt block
		terminator (brk next)
		addBlock next

	S.Return pos expr -> do
		curTyp <- gets curRetType 
		val <- case expr of
			Nothing -> do
				unless (curTyp == VoidType) (cmpErr pos "function requires return value")
				return Nothing
			Just ex -> do
				val <- cmpExpr ex
				unless (curTyp == typeOf val) (cmpErr pos "incorrect return type")
				return (Just val)

		terminator $ Do $ Ret val []
		addBlock =<< unique

cmpExpr :: S.Expr -> Cmp Operand
cmpExpr expr = case expr of
	S.Int pos n ->
		return $ cons (C.Int 64 $ toInteger n)

	S.Bool pos b ->
		return $ cons (C.Int 1 $ if b then 1 else 0)

	S.Ident pos name ->
		load =<< lookupSymbol pos name

	S.Call pos name args -> do
		op <- lookupSymbol pos name
		let typ = typeOf op
		case typ of
			FunctionType _ _ _ -> return ()
			_                  -> cmpErr pos (name ++ " isn't a function")

		unless (resultType typ == i64) $ cmpErr pos (name ++ " doesn't return i64")
		unless (argumentTypes typ == []) $ cmpErr pos (name ++ "has args")

		fmap fromJust (call op [])


	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		if isCons val1 && isCons val2 then
			return $ cons $ 
				let ConstantOperand cons1 = val1 in
				let ConstantOperand cons2 = val2 in
				case (typeOf (cons cons1), typeOf (cons cons2)) of
					(IntegerType 64, IntegerType 64) -> case op of
						S.Plus   -> C.Add False False cons1 cons2
						S.Minus  -> C.Sub False False cons1 cons2
						S.Times  -> C.Mul False False cons1 cons2
						S.Divide -> C.SDiv False cons1 cons2
						S.Mod    -> C.SRem cons1 cons2
		else
			case (typeOf val1, typeOf val2) of
				(IntegerType 64, IntegerType 64) -> do
					ins <- case op of
						S.Plus   -> return $ Add False False val1 val2 []
						S.Minus  -> return $ Sub False False val1 val2 []
						S.Times  -> return $ Mul False False val1 val2 []
						S.Divide -> return $ SDiv False val1 val2 []
						S.Mod    -> return $ SRem val1 val2 []
						_ -> throwError $ CmpError (pos, "i64 does not support operator")
					un <- unique
					instr (un := ins)
					return $ local (ptr i64) un
