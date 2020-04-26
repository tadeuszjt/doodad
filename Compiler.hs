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
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Short    as BSS

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab
import CmpState

import LLVM.AST hiding (function)
import LLVM.AST.Type hiding (void)
import LLVM.AST.Typed
import LLVM.AST.Name
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate
import LLVM.AST.Linkage
import LLVM.AST.CallingConvention
import LLVM.AST.Global
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module 
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C


mkBSS = BSS.toShort . BS.pack


type ModuleGen = ModuleBuilderT Cmp
type InstrGen  = IRBuilderT ModuleGen


look :: TextPos -> String -> ModuleGen Operand
look pos symbol = do
	(addr, ext) <- lift $ lookupSymbol pos symbol
	when (isJust ext) $ do
		isDec <- lift $ isDeclared symbol
		unless isDec $ do
			emitDefn (fromJust ext)
			lift (addDeclared symbol)
	return addr


l2 = lift . lift


codeGen :: CmpState -> S.AST -> (Either CmpError ((), [Definition]), CmpState)
codeGen cmpState ast =
	(runState . runExceptT . getCmp) (runModuleBuilderT emptyModuleBuilder cmp) cmpState
	where
		cmp :: ModuleGen ()
		cmp = do
			function (mkName ".main") [] VoidType $ \_ ->
				mapM_ cmpTopStmt ast

			return ()


cmpTopStmt :: S.Stmt -> InstrGen ()
cmpTopStmt stmt = case stmt of
	S.Print _ _ -> cmpStmt stmt
	S.CallStmt _ _ _ -> cmpStmt stmt

	S.Assign pos symbol expr -> do
		l2 (checkSymbolUndefined pos symbol)
		val <- cmpExpr expr
		fresh <- freshName (mkBSS symbol)
		let typ = typeOf val
		let ext = globalDef fresh typ Nothing

		l2 (addDeclared symbol)
		l2 (addExported fresh)

		if isCons val then do
			addr <- global fresh typ (toCons val)
			l2 $ addSymbol symbol addr (Just ext)
		else do
			addr <- global fresh typ (initOf typ)
			l2 $ addSymbol symbol addr (Just ext)
			void (store addr 0 val)

	
	S.Func pos symbol params retty stmts -> do
		l2 (checkSymbolUndefined pos symbol)
		name <- freshName (mkBSS symbol)

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
		l2 (addExported name)
		l2 $ addSymbol symbol op $ (Just ext)

		lift $ function name (zip paramTypes paramNames) returnType $ \args -> do

			l2 pushSymTab
			curRetType <- l2 getCurRetType
			l2 (setCurRetType returnType)

			forM (zip paramSymbols args) $ \(sym, arg) -> do
				op <- alloca (typeOf arg) Nothing 0
				store op 0 arg
				l2 (addSymbol sym op Nothing)

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
		l2 (addSymbol symbol op Nothing)

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
		void $ call op [(val, []) | val <- vals]

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs

		fmtArgs <- forM vals $ \val -> case typeOf val of
			IntegerType 1 -> do
				str <- globalStringPtr "true\0false" =<< freshName (mkBSS "boolstr")
				idx <- select val (int8 0) (int8 5)
				ptr <- gep (cons str) [idx]
				return ("%s", ptr)
			IntegerType _ ->
				return ("%d", val)
			t ->
				error ("can't print type: " ++ show t)

		let fmts = map fst fmtArgs
		let args = map snd fmtArgs
		fmt <- globalStringPtr (intercalate ", " fmts ++ "\n") =<< freshName (mkBSS "fmt")
		isDec <- l2 (isDeclared ".printf")
		unless isDec $ do
			l2 (addDeclared ".printf")
			void . lift $ externVarArgs (mkName "printf") [ptr i8] i32

		let op = cons $ C.GlobalReference (ptr $ FunctionType i32 [ptr i8] True) (mkName "printf")
		void $ call op $ [(arg, []) | arg <- (cons fmt):args]
	
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
	S.Int pos n ->
		return (int64 n)
	
	S.Bool pos b ->
		return $ bit (if b then 1 else 0)

	S.Ident pos symbol -> do
		addr <- lift (look pos symbol)
		load addr 0
		
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
	
	S.Prefix pos op expr -> do
		val <- cmpExpr expr
		case typeOf val of
			IntegerType 1 -> l2 (cmpErr pos "unsupported prefix")
			IntegerType n -> case op of
				S.Plus  -> return val
				S.Minus -> sub val (int8 1)
				_       -> l2 (cmpErr pos "unsupported prefix")

	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		case (typeOf val1, typeOf val2) of
			(IntegerType 1, IntegerType 1) -> do
				l2 (cmpErr pos "unsupported infix")
			(IntegerType aBits, IntegerType bBits) -> do
				unless (aBits == bBits) (l2 $ cmpErr pos "integer types don't match")
				(\f -> f val1 val2) $ case op of
					S.Plus    -> add 
					S.Minus   -> sub
					S.Times   -> mul 
					S.Divide  -> sdiv 
					S.Mod     -> srem
					S.LT      -> icmp SLT
					S.GT      -> icmp SGT
					S.LTEq    -> icmp SLE
					S.GTEq    -> icmp SGT
					S.EqEq    -> icmp EQ
