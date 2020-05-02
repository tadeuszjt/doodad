{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import           Control.Monad
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.List                  hiding (and, or)
import           Data.Word
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


mkBSS = BSS.toShort . BS.pack


data ValType
	= Void
	| I64
	| Bool
	| Tuple [ValType]
	| Func [ValType] ValType
	deriving (Show, Eq)

isFunc, isExpr :: ValType -> Bool
isFunc (Func _ _) = True
isFunc _          = False
isExpr I64        = True
isExpr Bool       = True
isExpr (Tuple _)  = True
isExpr _          = False
isTuple (Tuple _) = True
isTuple _         = False

fromASTType :: S.Type -> ValType
fromASTType typ = case typ of
	S.TBool    -> Bool
	S.I64      -> I64
	S.TTuple ts -> Tuple (map fromASTType ts)

opTypeOf :: ValType -> Type
opTypeOf typ = case typ of
	Void       -> VoidType
	I64        -> i64
	Bool       -> i1
	Tuple typs -> StructureType False (map opTypeOf typs)

zeroOf :: ValType -> C.Constant
zeroOf typ = case typ of
	I64        -> toCons (int64 0)
	Bool       -> toCons (bit 0)
	Tuple typs -> toCons $ struct Nothing False (map zeroOf typs)

data CompileState
	= CompileState
		{ curRetType :: ValType
		}
	deriving (Show, Eq)

initCompileState
	= CompileState
		{ curRetType = Void
		}


type Value   = (ValType, Operand)
type Compile = State CompileState
type Instr   = InstrCmpT ValType Compile
type Module  = ModuleCmpT ValType Compile



compile :: CmpState ValType -> S.AST -> Either CmpError ([Definition], CmpState ValType)
compile state ast =
	let (res, _) = runState (runModuleCmpT emptyModuleBuilder state cmp) initCompileState in
	fmap (\((_, defs), state') -> (defs, state')) res
	where
		cmp :: Module ()
		cmp =
			void $ function "main" [] VoidType $ \_ ->
				getInstrCmp (mapM_ cmpTopStmt ast)

cmpEquality :: TextPos -> S.Expr -> S.Expr -> Instr Operand
cmpEquality pos a b = do
	(aTyp, aOp) <- cmpExpr a
	(bTyp, bOp) <- cmpExpr b
	unless (aTyp == bTyp) (cmpErr pos "invalid comparison")
	opEquality aTyp aOp bOp
	where
		opEquality :: ValType -> Operand -> Operand -> Instr Operand
		opEquality typ aOp bOp = case typ of
			I64        -> (flip trunc) i1 =<< icmp EQ aOp bOp
			Bool       -> (flip trunc) i1 =<< icmp EQ aOp bOp
			Tuple typs -> do
				cnds <- forM (zip typs [0..]) $ \(ty, i) -> do
					fa <- extractValue aOp [i]
					fb <- extractValue bOp [i]
					opEquality ty fa fb
				cnd <- alloca i1 Nothing 0
				store cnd 0 (bit 1)
				forM_ cnds (\c -> store cnd 0 =<< and c =<< load cnd 0)
				load cnd 0
			_         -> cmpErr pos "invalid comparison"
		


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt (S.Assign pos pattern expr) = do
	assignPattern pos pattern =<< cmpExpr expr
	where
		assignPattern :: TextPos -> S.Pattern -> Value -> Instr ()
		assignPattern _   (S.PatIgnore _) _ = return ()
		assignPattern pos (S.PatTuple _ ps)  (Tuple ts, op) = do
			unless (length ps == length ts) (cmpErr pos "incorrect tuple length")
			forM_ (zip3 ps ts [0..]) $ \(p, t, i) -> do
				o <- extractValue op [fromIntegral i]
				assignPattern pos p (t, o)
		assignPattern pos (S.PatIdent _ symbol) (typ, op) = do
			checkUndefined pos symbol
			name <- freshName (mkBSS symbol)
			unless (isExpr typ) (cmpErr pos "isn't an expression")
			let opType = typeOf op
			loc <- global name opType (zeroOf typ)
			store loc 0 op
			addExtern symbol (globalDef name opType Nothing)
			addSymbol symbol (typ, loc)
			addDeclared symbol
			addExported symbol
		assignPattern pos _ _ =
			cmpErr pos "invalid pattern"
			
		



cmpTopStmt stmt = case stmt of
	S.Set _ _ _      -> cmpStmt stmt
	S.Print _ _      -> cmpStmt stmt
	S.CallStmt _ _ _ -> cmpStmt stmt
	S.Switch _ _ _   -> cmpStmt stmt
	

	S.Func pos symbol params retty stmts -> do
		checkUndefined pos symbol
		name <- freshName (mkBSS symbol)
		let Name nameStr = name
		
		let retType = maybe Void fromASTType retty
		paramTypes <- forM params $ \(S.Param _ paramName paramType) ->
			return $ case paramType of
				S.TBool -> Bool
				S.I64   -> I64

		let retOpType    = opTypeOf retType
		let paramSymbols = map S.paramName params
		let paramNames   = map mkName paramSymbols
		let paramNames'  = map (ParameterName . mkBSS) paramSymbols
		let paramOpTypes = map opTypeOf paramTypes

		let ext    = funcDef name (zip paramOpTypes paramNames) retOpType []
		let opType = FunctionType retOpType paramOpTypes False
		let op     = cons $ C.GlobalReference (ptr opType) name

		addDeclared symbol
		addExported symbol
		addExtern symbol ext
		addSymbol symbol (Func paramTypes retType, op)

		InstrCmpT $ IRBuilderT . lift $ function name (zip paramOpTypes paramNames') retOpType $
			\args -> (flip named) nameStr $ getInstrCmp $ do
				pushSymTab
				curRetType <- lift (gets curRetType)
				lift $ modify $ \s -> s { curRetType = retType }

				forM_ (zip4 paramSymbols paramTypes paramOpTypes args) $ \(sym, typ, opType, arg)-> do
					loc <- alloca opType Nothing 0
					store loc 0 arg
					addSymbol sym (typ, loc)
					
				mapM_ cmpStmt stmts
				popSymTab
				lift $ modify $ \s -> s { curRetType = curRetType }

		return ()
		

cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.Print pos exprs) = do
	prints =<< mapM cmpExpr exprs
	void (putchar '\n')
	where
		prints :: [Value] -> Instr ()
		prints []     = return ()
		prints [val]  = print val
		prints (v:vs) = print v >> printf ", " [] >> prints vs

		print :: Value -> Instr ()
		print (typ, op) = case typ of
			I64  -> void (printf "%ld" [op])

			Bool -> do
				str <- globalStringPtr "true\0false" =<< fresh
				idx <- select op (int64 0) (int64 5)
				ptr <- gep (cons str) [idx]
				void (printf "%s" [ptr])

			Tuple typs -> do
				putchar '('
				fields <- forM [0..length typs-1] $ \i ->
					extractValue op [toEnum i]

				prints (zip typs fields)
				putchar ')'
				return ()

			t -> cmpErr pos ("can't print type: " ++ show typ)

cmpStmt (S.Assign pos pattern expr) = do
	assignPattern pos pattern =<< cmpExpr expr
	where
		assignPattern :: TextPos -> S.Pattern -> Value -> Instr ()
		assignPattern _   (S.PatIgnore _) _ = return ()
		assignPattern pos (S.PatTuple _ ps)  (Tuple ts, op) = do
			unless (length ps == length ts) (cmpErr pos "incorrect tuple length")
			forM_ (zip3 ps ts [0..]) $ \(p, t, i) -> do
				o <- extractValue op [fromIntegral i]
				assignPattern pos p (t, o)
		assignPattern pos (S.PatIdent _ symbol) (typ, op) = do
			checkUndefined pos symbol
			name <- freshName (mkBSS symbol)
			unless (isExpr typ) (cmpErr pos "isn't an expression")
			loc <- alloca (typeOf op) Nothing 0
			store loc 0 op
			addSymbol symbol (typ, loc)
		assignPattern pos _ _ =
			cmpErr pos "invalid pattern"
		

cmpStmt (S.Assign pos pattern expr) = do
	let S.PatIdent _ symbol = pattern
	checkUndefined pos symbol
	(typ, op) <- cmpExpr expr
	loc <- alloca (typeOf op) Nothing 0
	store loc 0 op
	addSymbol symbol (typ, loc)

cmpStmt (S.Set pos symbol expr) = do
	(symType, loc) <- look pos symbol
	(exprType, op) <- cmpExpr expr
	unless (symType == exprType) (cmpErr pos "types don't match")

	void $ case symType of
		Bool -> store loc 0 op
		I64  -> store loc 0 op

cmpStmt (S.CallStmt pos symbol args) = do
	(typ, op) <- look pos symbol
	unless (isFunc typ) $ cmpErr pos (symbol ++ " isn't a function")
	vals <- mapM cmpExpr args
	return ()

cmpStmt (S.Return pos expr) = do
	curRetType <- lift (gets curRetType) 
	if isNothing expr then do
		unless (curRetType == Void) (cmpErr pos "cannot return void")
		retVoid
	else do
		(typ, op) <- cmpExpr (fromJust expr)
		unless (curRetType == typ) $ cmpErr pos ("incorrect type: " ++ show typ)
		ret op
	void block

cmpStmt (S.Block pos stmts) = do
	pushSymTab
	mapM_ cmpStmt stmts
	popSymTab

cmpStmt (S.If pos expr block els) = do
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

cmpStmt (S.Switch pos expr []) = return ()
cmpStmt (S.Switch pos expr cases) = do
	exit      <- freshUnName
	cndNames  <- replicateM (length cases) (freshName "case")
	stmtNames <- replicateM (length cases) (freshName "case_stmt")
	let nextNames = tail cndNames ++ [exit]
	let (caseExprs, stmts) = unzip cases

	br (head cndNames)
	pushSymTab

	forM_ (zip5 caseExprs cndNames stmtNames nextNames stmts) $
		\(caseExpr, cndName, stmtName, nextName, stmt) -> do
			emitBlockStart cndName
			if isJust caseExpr then do
				cnd <- cmpEquality pos expr (fromJust caseExpr)
				condBr cnd stmtName nextName
			else
				br stmtName
			emitBlockStart stmtName
			cmpStmt stmt
			br exit

	popSymTab
	br exit
	emitBlockStart exit


cmpExpr :: S.Expr -> Instr (ValType, Operand)
cmpExpr expr = case expr of
	S.Int pos i  -> return (I64, int64 i)
	S.Bool pos b -> return (Bool, bit $ if b then 1 else 0)

	S.Ident pos symbol -> do
		(typ, loc) <- look pos symbol
		unless (isExpr typ) $ cmpErr pos (symbol ++ " isn't an expression")
		op <- load loc 0
		return (typ, op)
	

	S.Call pos symbol args -> do
		(typ, op) <- look pos symbol
		unless (isFunc typ) $ cmpErr pos (symbol ++ " isn't a function")
		let Func argTypes retType = typ
		vals <- mapM cmpExpr args
		let (typs, ops) = unzip vals
		unless (argTypes == typs) (cmpErr pos "arg types don't match")
		ret <- call op (map (,[]) ops)
		return (retType, ret)
	

	S.Tuple pos [arg] -> cmpExpr arg
	S.Tuple pos args  -> do
		(typs, ops) <- fmap unzip (mapM cmpExpr args)
		let tupOpType = StructureType False (map typeOf ops)
		loc <- alloca tupOpType Nothing 0

		forM_ (zip3 typs ops [0..]) $ \(typ, op, i) -> do
			loc' <- load loc 0
			agg <- insertValue loc' op [i]
			store loc 0 agg

		op <- load loc 0
		return (Tuple typs, op)
		

	S.TupleIndex pos tup idx -> do
		(typ, op) <- cmpExpr tup
		unless (isTuple typ) (cmpErr pos "expr isn't tuple")
		let Tuple typs = typ
		unless (idx >= 0 && idx < length typs) (cmpErr pos "tuple index out of range")
		field <- extractValue op [fromIntegral idx]
		return (typs !! idx, field)


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

	_ -> error (show expr)
