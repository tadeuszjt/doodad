{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import Control.Monad.Fail
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Char

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab
import CmpState

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.CallingConvention
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C


codeGen :: CmpState -> S.AST -> (Either CmpError (), CmpState)
codeGen cmpState ast =
	runState (runExceptT $ getCmp cmp) cmpState
	where
		cmp = do
			addBlock $ BasicBlock (mkName "entry") [] (Do $ Ret Nothing [])
			mapM_ cmpTopStmt ast


cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Set pos name expr -> cmpStmt stmt
	S.Print pos exprs -> cmpStmt stmt
	S.Block pos stmts -> cmpStmt stmt
	S.Call pos name args -> cmpStmt stmt

	S.Assign pos name expr -> do
		checkSymbolIsFree pos name

		val <- cmpExpr expr
		unless (isCons val) (cmpErr pos "const only")
		let typ = typeOf val

		unName <- uniqueName name
		let op = cons $ global (ptr typ) unName
		let def = globalVar unName False typ 

		addSymbol name op $ Just (unName, def Nothing)
		addDeclared unName
		addExported unName
		addGlobal $ def (Just $ toCons val)

	S.Func pos name block -> do
		checkSymbolIsFree pos name

		pushBlocks
		addBlock $ BasicBlock (mkName "entry") [] (Do $ Ret Nothing [])
		cmpStmt block
		blocks <- popBlocks

		unName <- uniqueName name

		let fnRetType  = void
		let fnArgTypes = []
		let fnType     = FunctionType fnRetType fnArgTypes False

		let op = cons $ global (ptr fnType) unName
		let ext = GlobalDefinition $ functionDefaults
			{ G.name       = unName
			, G.returnType = fnRetType
			}
		let def = GlobalDefinition $ functionDefaults
			{ G.name        = unName
			, G.returnType  = fnRetType
			, G.basicBlocks = blocks
			}

		addSymbol name op $ Just (unName, ext)
		addDeclared unName
		addExported unName
		addGlobal def


cmpStmt :: S.Stmt -> Cmp ()
cmpStmt stmt = case stmt of
	S.Assign pos name expr -> do
		checkSymbolIsFree pos name

		val <- cmpExpr expr
		let typ = typeOf val

		unName <- uniqueName name
		let op = local (ptr typ) unName
		addSymbol name op Nothing

		instr $ unName := Alloca typ Nothing 0 []
		instr $ Do $ Store False op val Nothing 0 []

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs

		let fmts = (flip map) vals $ \val ->
			case typeOf val of
				IntegerType 32 -> "%d"

		fmtCons <- stringDef (intercalate ", " fmts ++ "\n")

		let printfName = mkName "printf"
		let printfTyp = FunctionType i32 [ptr i8] True
		let printfOp  = cons $ global (ptr printfTyp) printfName
		let args      = (cons fmtCons, []) : [ (op, []) | op <- vals ]

		decs <- gets declared
		unless (printfName `elem` decs) (addDeclared printfName >> addGlobal printfFn)
		instr $ Do $ Call Nothing C [] (Right printfOp) args [] []

	S.Set pos name expr -> do
		op <- lookupSymbol pos name
		val <- cmpExpr expr
		instr $ Do $ Store False op val Nothing 0 []
	
	S.Block pos block -> do
		pushSymTab
		mapM_ cmpStmt block
		popSymTab

	S.Call pos name args -> do
		op <- lookupSymbol pos name
		let typ = typeOf op
		case typ of
			FunctionType _ _ _ -> return ()
			_                  -> cmpErr pos (name ++ " isn't a function")

		let FunctionType retType argTypes isVarg = typ
		instr $ Do $ Call Nothing C [] (Right op) [] [] [] 


cmpExpr :: S.Expr -> Cmp Operand
cmpExpr expr = case expr of
	S.Int pos n ->
		return $ cons (C.Int 32 $ toInteger n)

	S.Ident pos name -> do
		op <- lookupSymbol pos name 
		un <- unique
		instr $ un := Load False op Nothing 0 []
		return $ local (ptr $ typeOf op) un

	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		if isCons val1 && isCons val2 then
			return $ cons $ 
				let ConstantOperand cons1 = val1 in
				let ConstantOperand cons2 = val2 in
				case (typeOf (cons cons1), typeOf (cons cons2)) of
					(IntegerType 32, IntegerType 32) -> case op of
						S.Plus   -> C.Add False False cons1 cons2
						S.Minus  -> C.Sub False False cons1 cons2
						S.Times  -> C.Mul False False cons1 cons2
						S.Divide -> C.SDiv False cons1 cons2
						S.Mod    -> C.SRem cons1 cons2
		else
			case (typeOf val1, typeOf val2) of
				(IntegerType 32, IntegerType 32) -> do
					ins <- case op of
						S.Plus   -> return $ Add False False val1 val2 []
						S.Minus  -> return $ Sub False False val1 val2 []
						S.Times  -> return $ Mul False False val1 val2 []
						S.Divide -> return $ SDiv False val1 val2 []
						S.Mod    -> return $ SRem val1 val2 []
						_ -> throwError $ CmpError (pos, "i32 does not support operator")
					un <- unique
					instr (un := ins)
					return (local i32 un)


stringDef :: String -> Cmp C.Constant
stringDef str = do
	let chars     = map (C.Int 8 . toInteger . ord) (str ++ "\0")
	let strArr    = C.Array i8 chars
	let strArrTyp = typeOf (cons strArr)

	strName <- unique
	let strRef = global (ptr strArrTyp) strName
	addGlobal $ globalVar strName True strArrTyp (Just strArr)
	return $ C.BitCast (C.GetElementPtr False strRef []) (ptr i8)


typeOf :: Operand -> Type
typeOf (LocalReference (PointerType typ _) _) = typ
typeOf (ConstantOperand cons) = case cons of
	C.GlobalReference (PointerType typ _) _ -> typ
	C.Int nb _                              -> IntegerType nb
	C.Array typ elems                       -> ArrayType (fromIntegral $ length elems) typ
	C.Add _ _ a b                           -> typeOf (ConstantOperand a)
	C.Sub _ _ a b                           -> typeOf (ConstantOperand a)
	C.Mul _ _ a b                           -> typeOf (ConstantOperand a)
	C.SDiv _ a b                            -> typeOf (ConstantOperand a)
	C.SRem a b                              -> typeOf (ConstantOperand a)



isCons :: Operand -> Bool
isCons (ConstantOperand _) = True
isCons _                   = False


toCons :: Operand -> C.Constant
toCons (ConstantOperand c) = c


initOf :: Type -> C.Constant
initOf (IntegerType 32) = C.Int 32 0


global :: Type -> Name -> C.Constant
global = C.GlobalReference


local :: Type -> Name -> Operand
local = LocalReference


cons :: C.Constant -> Operand
cons = ConstantOperand


globalVar :: Name -> Bool -> Type -> Maybe C.Constant -> Definition
globalVar name isCons typ init = GlobalDefinition $ globalVariableDefaults
	{ G.name        = name
	, G.isConstant  = isCons
	, G.type'       = typ
	, G.initializer = init
	}


mainFn :: Name -> [Named Instruction] -> Definition
mainFn name ins = GlobalDefinition $ functionDefaults
	{ G.returnType  = void
	, G.name        = name
	, G.basicBlocks = [BasicBlock (mkName "entry") ins (Do $ Ret Nothing [])]
	}


printfFn :: Definition
printfFn = GlobalDefinition $ functionDefaults
	{ G.returnType = i32
	, G.name       = mkName "printf"
	, G.parameters = ([Parameter (ptr i8) (mkName "fmt") []], True)
	}

