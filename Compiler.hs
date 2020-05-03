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
	| I32
	| I64
	| Bool
	| Char
	| String
	| Array Int ValType
	| Tuple [ValType]
	| Func [ValType] ValType
	deriving (Show, Eq)

isFunc, isFirst, isTuple :: ValType -> Bool
isFunc (Func _ _)   = True
isFunc _            = False
isFirst I32          = True
isFirst I64          = True
isFirst Bool         = True
isFirst Char         = True
isFirst String       = True
isFirst (Tuple _)    = True
isFirst (Array _ _)  = True
isFirst _            = False
isArray (Array _ _) = True
isArray _           = False
isTuple (Tuple _)   = True
isTuple _           = False

fromASTType :: S.Type -> ValType
fromASTType typ = case typ of
	S.TBool     -> Bool
	S.TI32      -> I32
	S.TI64      -> I64
	S.TChar     -> Char
	S.TString   -> String
	S.TArray n t -> Array n (fromASTType t)
	S.TTuple ts -> Tuple (map fromASTType ts)

opTypeOf :: ValType -> Type
opTypeOf typ = case typ of
	Void       -> VoidType
	I32        -> i32
	I64        -> i64
	Bool       -> i1
	Char       -> i32
	String     -> ptr i8
	Array n e  -> ArrayType (fromIntegral n) (opTypeOf e)
	Tuple typs -> StructureType False (map opTypeOf typs)

zeroOf :: ValType -> C.Constant
zeroOf typ = case typ of
	I64        -> toCons (int64 0)
	Bool       -> toCons (bit 0)
	Char       -> toCons (int32 0)
	String     -> C.IntToPtr (toCons $ int64 0) (ptr i8)
	Array n t  -> toCons $ array $ replicate n (zeroOf t)
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


valsEqual :: TextPos -> Value -> Value -> Instr Value
valsEqual pos (aTyp, aOp) (bTyp, bOp) = do
	unless (aTyp == bTyp) (cmpErr pos "typs don't match")
	op <- opEquality aTyp aOp bOp
	return (Bool, op)
	where
		opEquality :: ValType -> Operand -> Operand -> Instr Operand
		opEquality typ aOp bOp = case typ of
			I64      -> icmp EQ aOp bOp
			Bool     -> icmp EQ aOp bOp
			Char     -> icmp EQ aOp bOp
			String   -> icmp EQ (int32 0) =<< strcmp aOp bOp
			Array n t -> do
				cnd <- alloca i1 Nothing 0
				store cnd 0 (bit 1)
				for (int64 $ fromIntegral n) $ \i -> do
					pa <- gep aOp [int64 0, i]
					pb <- gep bOp [int64 0, i]
					a <- load pa 0
					b <- load pb 0
					c <- opEquality t a b
					store cnd 0 =<< and c =<< load cnd 0
				load cnd 0
			Tuple ts -> do
				cnds <- forM (zip ts [0..]) $ \(t, i) -> do
					fa <- extractValue aOp [i]
					fb <- extractValue bOp [i]
					opEquality t fa fb
				cnd <- alloca i1 Nothing 0
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
			unless (isFirst typ) (cmpErr pos "isn't an expression")
			let opType = typeOf op
			loc <- global name opType (zeroOf typ)
			store loc 0 op
			addExtern symbol (globalDef name opType Nothing)
			addSymbol symbol (typ, loc)
			addDeclared symbol
			addExported symbol
		assignPattern pos _ _ =
			cmpErr pos "invalid pattern"
			
cmpTopStmt (S.Extern pos symbol params retty) = do
	checkUndefined pos symbol
	let name      = mkName symbol

	let retType = maybe Void fromASTType retty
	unless (isFirst retType) (cmpErr pos "return isn't expression type")
	let retOpType = opTypeOf retType

	paramList <- forM params $ \(S.Param pos name typ) -> do
		let paramType = fromASTType typ
		unless (isFirst paramType) (cmpErr pos "param isn't an expression")
		return (paramType, opTypeOf paramType)
	
	let (paramTypes, paramOpTypes) = unzip paramList
	let typ = Func paramTypes retType

	op <- ensureExtern symbol paramOpTypes retOpType False
	addSymbol symbol (typ, op)
    
	return ()


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
		unless (retType == Void || isFirst retType) $
			cmpErr pos "return type isn't an expression"

		paramTypes <- forM params $ \(S.Param pos paramName paramType) -> do
			let typ = fromASTType paramType
			unless (isFirst typ) (cmpErr pos "param type isn't an expression")
			return typ

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
			I32  -> void (printf "%d" [op])
			I64  -> void (printf "%ld" [op])
			Char -> void (putchar' op)
			String -> void (printf "%s" [op])

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

			Array n t -> do
				putchar '['
				vals <- forM [0..n-1] $ \i -> do
					ptr <- gep op [int64 0, int64 (fromIntegral i)]
					elm <- load ptr 0
					return (t, elm)
				prints vals
				void (putchar ']')
				


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
			unless (isFirst typ) (cmpErr pos "isn't an expression")
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
	let Func paramTypes retType = typ
	(argTypes, argOps) <- fmap unzip (mapM cmpExpr args)
	unless (argTypes == paramTypes) (cmpErr pos "arg types don't match")
	void $ call op (map (,[]) argOps)


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
				val <- cmpExpr expr
				cas <- cmpExpr (fromJust caseExpr)
				(_, cnd) <- valsEqual pos val cas
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
	S.Char pos c -> return (Char, int32 $ fromIntegral $ fromEnum c)

	S.Ident pos symbol -> do
		(typ, loc) <- look pos symbol
		unless (isFirst typ) $ cmpErr pos (symbol ++ " isn't an expression")
		op <- load loc 0
		return (typ, op)
	
	S.String pos str -> do
		ptr <- globalStringPtr str =<< fresh
		return (String, cons ptr)

	S.Call pos symbol args -> do
		(typ, op) <- look pos symbol
		unless (isFunc typ) $ cmpErr pos (symbol ++ " isn't a function")
		let Func argTypes retType = typ
		vals <- mapM cmpExpr args
		let (typs, ops) = unzip vals
		unless (argTypes == typs) (cmpErr pos "arg types don't match")
		ret <- call op (map (,[]) ops)
		return (retType, ret)
	
	S.Constructor pos newASTType expr -> do
		let newType = fromASTType newASTType
		unless (isFirst newType) $ cmpErr pos ("isn't expression type")
		(typ, op) <- cmpExpr expr
		unless (isFirst newType) $ cmpErr pos ("isn't an expression")
		newOp <- case (newType, typ) of
			(I32, I64)  -> trunc op i32
			(I32, Char) -> return op
			(Char, I32) -> return op
			(Char, I64) -> trunc op i32

		return (newType, newOp)

	S.Array pos exprs -> do
		let num = length exprs
		unless (num > 0) (cmpErr pos "can't deduce array type")
		(types, ops) <- fmap unzip (mapM cmpExpr exprs)
		let elemType = head types
		unless (isFirst elemType) (cmpErr pos "element type isn't expression")
		unless (all (== elemType) types) (cmpErr pos "element types don't match")
		name <- fresh
		let typ = Array num elemType
		op <- global name (opTypeOf typ) (zeroOf typ)
		forM_ (zip ops [0..]) $ \(o, i) -> do
			loc <- gep op [int64 0, int64 i]
			store loc 0 o

		return (typ, op)

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
		let invalid = cmpErr pos "invalid infix"
		val1@(typ1, op1) <- cmpExpr expr1
		val2@(typ2, op2) <- cmpExpr expr2

		case (typ1, typ2) of
			(Bool, Bool) ->
				case operator of
					S.EqEq   -> fmap (Bool,) (icmp EQ op1 op2)
					S.AndAnd -> fmap (Bool,) (and op1 op2)
					S.OrOr   -> fmap (Bool,) (or op1 op2)
					_        -> invalid

			(I64, I64) ->
				case operator of
					S.Plus   -> fmap (I64,) (add op1 op2)
					S.Minus  -> fmap (I64,) (sub op1 op2)
					S.Times  -> fmap (I64,) (mul op1 op2)
					S.Divide -> fmap (I64,) (sdiv op1 op2)
					S.Mod    -> fmap (I64,) (srem op1 op2)
					S.LT     -> fmap (Bool,) $ icmp SLT op1 op2 
					S.GT     -> fmap (Bool,) $ icmp SGT op1 op2
					S.LTEq   -> fmap (Bool,) $ icmp SLE op1 op2
					S.GTEq   -> fmap (Bool,) $ icmp SGT op1 op2
					S.EqEq   -> fmap (Bool,) $ icmp EQ op1 op2
					_        -> invalid
			(Char, Char) ->
				case operator of
					S.EqEq   -> fmap (Bool,) $ icmp EQ op1 op2
					S.LT     -> fmap (Bool,) $ icmp SLT op1 op2 
					_        -> invalid
			(String, String) ->
				case operator of
					S.EqEq   -> valsEqual pos val1 val2
					_        -> invalid
			(Tuple _, Tuple _) ->
				case operator of
					S.EqEq   -> valsEqual pos val1 val2
					_        -> invalid
			(Array _ _, Array _ _) ->
				case operator of
					S.EqEq   -> valsEqual pos val1 val2
					_        -> invalid
			(ta, tb) -> invalid

	_ -> error (show expr)
