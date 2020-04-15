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
	runState (runExceptT $ getCmp $ mapM_ cmpTopStmt ast) cmpState


type Extern = (Name, Definition)


newtype CmpError
	= CmpError { getCmpError :: (L.AlexPosn, String) }
	deriving (Show)


data CmpState
	= CmpState
		{ symTab       :: SymTab.SymTab String (Operand, Maybe Extern)
		, nameSupply   :: Map.Map String Int
		, uniqueCount  :: Word
		, declared     :: Set.Set Name
		, exported     :: Set.Set Name
		, globals      :: [Definition]
		, instructions :: [Named Instruction]
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState)


initCmpState = CmpState
	{ symTab       = SymTab.initSymTab
	, nameSupply   = Map.fromList $ [("printf", 1), ("main", 1)]
	, uniqueCount  = 0
	, declared     = Set.empty
	, exported     = Set.empty
	, globals      = []
	, instructions = []
	}


cmpErr :: L.AlexPosn -> String -> Cmp a
cmpErr pos str = throwError $ CmpError (pos, str)


unique :: Cmp Name
unique = do
	count <- gets uniqueCount
	modify $ \s -> s { uniqueCount = count + 1 }
	return (UnName count)


uniqueName :: String -> Cmp Name
uniqueName name = do
	names <- gets nameSupply
	let (x', name') = case Map.lookup name names of
		Just x  -> (x+1, name ++ "." ++ show x)
		Nothing -> (1, name)
	modify $ \s -> s { nameSupply = Map.insert name x' names }
	return (mkName name')


addDeclared :: Name -> Cmp ()
addDeclared name =
	modify $ \s -> s { declared = Set.insert name (declared s) }
	  

addExported :: Name -> Cmp ()
addExported name =
	modify $ \s -> s { exported = Set.insert name (exported s) }


addGlobal :: Definition -> Cmp ()
addGlobal glob =
	modify $ \s -> s { globals = glob : (globals s) }


addSymbol :: String -> (Operand, Maybe Extern) -> Cmp ()
addSymbol name (op, extern) =
	modify $ \s -> s { symTab = SymTab.insert name (op, extern) (symTab s) }


instr :: Instruction -> Cmp Name
instr ins = do
	un <- unique
	modify $ \s -> s { instructions = (un := ins) : (instructions s) }
	return un


doInstr :: Instruction -> Cmp ()
doInstr ins = 
	modify $ \s -> s { instructions = (Do ins) : (instructions s) }


lookupSymbol :: L.AlexPosn -> String -> Cmp Operand
lookupSymbol pos name = do
	st <- gets symTab
	case SymTab.lookup name st of
		Nothing           -> cmpErr pos (name ++ " doesn't exist")
		Just (op, extern) -> do
			case extern of
				Nothing        -> return ()
				Just (nm, def) -> gets declared >>= \decls ->
					unless (nm `elem` decls) (addGlobal def >> addDeclared nm)
			return op


cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Assign pos name expr -> do
		st <- gets symTab
		case SymTab.lookup name [head st] of
			Just _  -> cmpErr pos (name ++ " already defined")
			Nothing -> return ()

		val <- cmpExpr expr
		unless (isCons val) (cmpErr pos "const only")
		let typ = typeOf val

		unName <- uniqueName name
		let op = cons $ global (ptr typ) unName
		let def = globalVar unName False typ 

		addSymbol name (op, Just (unName, def Nothing))
		addDeclared unName
		addExported unName
		addGlobal $ def (Just $ toCons val)

	S.Set pos name expr -> do
		op <- lookupSymbol pos name
		val <- cmpExpr expr
		doInstr $ Store False op val Nothing 0 []
			
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
		doInstr $ Call Nothing C [] (Right printfOp) args [] []


cmpExpr :: S.Expr -> Cmp Operand
cmpExpr expr = case expr of
	S.Int pos n ->
		return $ cons (C.Int 32 $ toInteger n)

	S.Ident pos name -> do
		op <- lookupSymbol pos name 
		ref <- instr $ Load False op Nothing 0 []
		return $ local (typeOf op) ref 

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
					fmap (local i32) (instr ins)


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
typeOf (LocalReference typ _) = typ
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

