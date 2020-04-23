{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Prelude hiding (EQ)
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
import LLVM.IRBuilder.Monad
import qualified LLVM.IRBuilder.Module as IR 
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C


codeGen :: CmpState -> S.AST -> ((Either CmpError (), [Definition]), CmpState)
codeGen cmpState ast =
	runState (IR.runModuleBuilderT IR.emptyModuleBuilder $ runExceptT $ getCmp cmp) cmpState
	where
		cmp = do
			mapM_ reserveName ["printf", "puts"]
			let printfName = mkName "printf"
			let printfOp  = global (ptr $ FunctionType i32 [ptr i8] True) printfName
			let printfExt = funcDef printfName i32 [(mkName "fmt", ptr i8)] True []
			addSymbol ".printf" printfOp $ Just (printfName, printfExt)

			let boolStrName  = mkName ".boolStr"
			let boolStrArray = C.Array i8 $ map (C.Int 8 . toInteger . ord) "false\x00true\x00"
			let boolStrTyp   = typeOf (cons boolStrArray)
			let boolStrOp    = global (ptr boolStrTyp) boolStrName
			let boolStr      = globalVar boolStrTyp boolStrName True (Just boolStrArray)
			addSymbol ".boolStr" boolStrOp $ Just (boolStrName, boolStr)

			addBlock (mkName "entry") 
			mapM_ cmpTopStmt ast


cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Set pos name expr -> cmpStmt stmt
	S.Print pos exprs -> cmpStmt stmt
	S.Block pos stmts -> cmpStmt stmt
	S.CallStmt pos name args -> cmpStmt stmt
	S.If pos expr block els -> cmpStmt stmt

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
			void $ IR.global unName typ (toCons val)
		else do
			op <- IR.global unName typ (initOf typ)
			store op val

	S.Func pos name params retType block -> do
		checkSymbolUndefined pos name
		unName <- uniqueName name

		let mapType = \t -> case t of
			S.I64 -> i64
			S.TBool -> i1

		let fnRetType = case retType of
			Nothing -> VoidType
			Just t  -> mapType t

		let fnType  = FunctionType fnRetType (map (mapType . S.paramType) params) False
		let op      = global (ptr fnType) unName
		addSymbol name op Nothing
		addDeclared unName

		pushSymTab
		params' <- forM params $ \(S.Param pos name typ) -> do
			checkSymbolUndefined pos name
			unName <- uniqueName name
			let typ' = mapType typ
			addSymbol name (local (typ') unName) Nothing
			return (unName, typ')
			
		pushBlocks
		curTyp <- gets curRetType
		modify $ \s -> s { curRetType = fnRetType }
		addBlock (mkName "entry")
		cmpStmt block
		blocks <- popBlocks
		modify $ \s -> s { curRetType = curTyp }
		popSymTab

		let ext = funcDef unName fnRetType params' False []
		addSymbol name op $ Just (unName, ext)
		addExported unName
		IR.emitDefn $ funcDef unName fnRetType params' False blocks
		--void $ IR.function unName [] fnRetType (\_ -> return ()) 


cmpStmt :: S.Stmt -> Cmp ()
cmpStmt stmt = case stmt of
	S.Assign pos name expr -> do
		checkSymbolUndefined pos name
		unName <- uniqueName name

		val <- cmpExpr expr
		let typ = typeOf val
		let op = local typ unName

		addSymbol name op Nothing
		alloca typ unName
		store op val

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs

		fmtArgs <- forM vals $ \val -> case typeOf val of
			IntegerType 1 -> do
				boolStr <- lookupSymbol pos ".boolStr"
				boolPtr <- subscript boolStr =<< select val (cons $ C.Int 8 0) (cons $ C.Int 8 6)
				return ("%s", boolPtr)

			IntegerType _ ->
				return ("%d", val)

			t ->
				error ("can't print type: " ++ show t)

		let fmts = map fst fmtArgs
		let args = map snd fmtArgs

		fmt <- bitcast (ptr i8) =<< string (intercalate ", " fmts ++ "\n")
		op <- lookupSymbol pos ".printf"
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
		(retType, paramTypes) <- case typ of
			FunctionType r pt _ -> return (r, pt)
			_                   -> cmpErr pos (name ++ " isn't a function")

		vals <- mapM cmpExpr args
		unless (paramTypes == map typeOf vals) $ cmpErr pos "arg types don't match"
		void $ call op vals

	S.If pos expr block els -> do
		cnd <- cmpExpr expr
		unless (typeOf cnd == i1) $ cmpErr pos "expression isn't boolean"
		true  <- uniqueName "if.true"
		false <- uniqueName "if.false"
		next  <- uniqueName "if.next"
		terminator (cndBr cnd true false)
		addBlock true
		cmpStmt block
		terminator (brk next)
		addBlock false
		unless (isNothing els) $ cmpStmt (fromJust els)
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

	S.Ident pos name -> do
		op <- lookupSymbol pos name
		if isPtr op then load op else return op

	S.Call pos name args -> do
		op <- lookupSymbol pos name
		let typ = typeOf op
		(retType, paramTypes) <- case typ of
			FunctionType r pt _ -> return (r, pt)
			_                   -> cmpErr pos (name ++ " isn't a function")

		vals <- mapM cmpExpr args
		let typs = map typeOf vals

		unless (retType /= VoidType) $ cmpErr pos (name ++ "void function in expression")
		unless (paramTypes == typs) $ cmpErr pos "arg types don't match"

		fmap fromJust (call op vals)


	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		case (typeOf val1, typeOf val2) of
			(IntegerType aBits, IntegerType bBits) -> do
				unless (aBits == bBits) $ cmpErr pos "integer types don't match"
				case op of
					S.Plus    -> add val1 val2
					S.Minus   -> sub val1 val2
					S.Times   -> mul val1 val2
					S.Divide  -> idiv val1 val2
					S.Mod     -> imod val1 val2
					S.LT      -> icmp val1 val2 SLT
					S.GT      -> icmp val1 val2 SGT
					S.LTEq    -> icmp val1 val2 SLE
					S.GTEq    -> icmp val1 val2 SGT
					S.EqEq    -> icmp val1 val2 EQ
