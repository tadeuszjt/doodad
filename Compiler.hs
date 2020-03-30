{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
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


codeGen :: CmpState -> S.Stmt -> (Either CmpError ([Definition], Definition), CmpState)
codeGen cmpState stmt =
	runState (runExceptT $ getCmp $ cmpTopStmt stmt) cmpState


newtype CmpError
	= CmpError { getCmpError :: (L.AlexPosn, String) }
	deriving (Show)


data CmpState
	= CmpState
		{ symTab       :: SymTab.SymTab String Operand
		, nameSupply   :: Map.Map String Int
		, uniqueCount  :: Word
		, externs      :: [Definition]
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState)


initCmpState = CmpState
	{ symTab       = SymTab.initSymTab
	, nameSupply   = Map.fromList $ [("printf", 1), ("main", 1)]
	, uniqueCount  = 0
	, externs      = []
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


addExtern :: Definition -> Cmp ()
addExtern def = do
	exts <- gets externs
	unless (def `elem` exts) $ modify $ \s -> s { externs = (externs s) ++ [def] }


addSymbol :: String -> Operand -> Cmp ()
addSymbol name op =
	modify $ \s -> s { symTab = SymTab.insert name op (symTab s) }


lookupSymbol :: L.AlexPosn -> String -> Cmp Operand
lookupSymbol pos name = do
	st <- gets symTab
	case SymTab.lookup name st of
		Nothing -> throwError $ CmpError (pos, name ++ " doesn't exist")
		Just op -> return op


cmpTopStmt :: S.Stmt -> Cmp ([Definition], Definition)
cmpTopStmt stmt = case stmt of
	S.Assign pos name expr -> do
		[st] <- gets symTab
		case SymTab.lookup name [st] of
			Just _  -> cmpErr pos (name ++ " already defined")
			Nothing -> return ()

		val <- cmpExpr expr
		unless (isCons val) (cmpErr pos "const only")
		let typ = typeOf val

		unName <- uniqueName name
		addSymbol name (cons $ global (ptr typ) unName)

		let def = globalVar unName False typ 
		addExtern (def Nothing)
		return ([], def $ Just (toCons val))

--	S.Set pos name expr -> do
--		op <- lookupSymbol pos name
--		val <- cmpExpr expr
--		unless (isCons val) (cmpErr pos "const only")
--		unName <- uniqueName "main"
--		let ins = Do $ Store False op val Nothing 0 []
--		return ([], mainFn unName [ins])
			
	S.Print pos exprs -> do
		addExtern printfFn

		vals <- mapM cmpExpr exprs
		(fmtDef, fmtCons) <- stringDef $ (++ "\n") $ intercalate "," $ (flip map) vals $ \val -> case typeOf val of
			IntegerType 32 -> "%d"


		let printfTyp = FunctionType i32 [ptr i8] True
		let printfOp  = cons $ global (ptr printfTyp) (mkName "printf")
		let args      = (cons fmtCons, []) : [ (op, []) | op <- vals ]

		unName <- uniqueName "main"
		ref <- unique
		let ins = ref := Call Nothing C [] (Right printfOp) args [] []
		let ret = Do $ Ret (Just $ local i32 ref) []
		return ([printfFn, fmtDef], mainFn unName [ins] ret)


cmpExpr :: S.Expr -> Cmp Operand
cmpExpr expr = case expr of
	S.Int pos n ->
		return $ cons (C.Int 32 $ toInteger n)

--	S.Ident pos name -> do
--		op <- lookupName pos name 
--		ref <- unique
--
--		addInstr $ loadRef := Load False op Nothing 0 []
--		return $ local (typeOf op) loadRef
--
--	S.Infix pos op expr1 expr2 -> do
--		val1 <- cmpExpr expr1
--		val2 <- cmpExpr expr2
--
--		if isCons val1 && isCons val2
--		then return $ cons $ 
--			let ConstantOperand cons1 = val1 in
--			let ConstantOperand cons2 = val2 in
--			case (typeOf (cons cons1), typeOf (cons cons2)) of
--				(IntegerType 32, IntegerType 32) -> case op of
--					S.Plus   -> C.Add False False cons1 cons2
--					S.Minus  -> C.Sub False False cons1 cons2
--					S.Times  -> C.Mul False False cons1 cons2
--					S.Divide -> C.SDiv False cons1 cons2
--					S.Mod    -> C.SRem cons1 cons2
--		else do
--			ref <- unique
--			case (typeOf val1, typeOf val2) of
--				(IntegerType 32, IntegerType 32) -> do
--					ins <- case op of
--						S.Plus   -> return $ Add False False val1 val2 []
--						S.Minus  -> return $ Sub False False val1 val2 []
--						S.Times  -> return $ Mul False False val1 val2 []
--						S.Divide -> return $ SDiv False val1 val2 []
--						S.Mod    -> return $ SRem val1 val2 []
--						_ -> throwError $ CmpError (pos, "i32 does not support operator")
--					addInstr (ref := ins)
--					return (local i32 ref)


stringDef :: String -> Cmp (Definition, C.Constant)
stringDef str = do
	let chars     = map (C.Int 8 . toInteger . ord) (str ++ "\0")
	let strArr    = C.Array i8 chars
	let strArrTyp = typeOf (cons strArr)

	strName <- unique
	let def =  globalVar strName True strArrTyp (Just strArr)
	let strRef = global (ptr strArrTyp) strName
	return (def, C.BitCast (C.GetElementPtr False strRef []) (ptr i8))


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


mainFn :: Name -> [Named Instruction] -> Named Terminator -> Definition
mainFn name ins ter = GlobalDefinition $ functionDefaults
	{ G.returnType  = i32 
	, G.name        = name
	, G.basicBlocks = [BasicBlock (mkName "entry") ins ter]
	}


printfFn :: Definition
printfFn = GlobalDefinition $ functionDefaults
	{ G.returnType = i32
	, G.name       = mkName "printf"
	, G.parameters = ([Parameter (ptr i8) (mkName "fmt") []], True)
	}

