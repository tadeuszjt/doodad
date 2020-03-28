{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Data.Char
import Data.List
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import LLVM.Context
import LLVM.ExecutionEngine
import LLVM.PassManager
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.Module       as M

foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> (IO ())


codeGen :: CmpState -> S.AST -> IO (Either CmpError CmpState)
codeGen cmpState ast = do
	let (res, cmpState') = runState (runExceptT $ getCmp cmp) cmpState
	case res of
		Left cmpErr -> return (Left cmpErr)
		Right _     -> do
			optMod <- runJIT (llvmModule cmpState') 
			return $ Right (cmpState' { llvmModule = optMod })

	where
		cmp = do
			mapM_ cmpTopStmt ast
			addDef . mainFn =<< gets instructions


run :: FunPtr a -> IO ()
run fn = haskFun (castFunPtr fn :: FunPtr (IO ()))


passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }


runJIT :: Module -> IO Module
runJIT astMod =
	withContext $ \ctx ->
		withMCJIT ctx optLevel model ptrelim fastins $ \ee -> 
			M.withModuleFromAST ctx astMod $ \mod -> do
				withModuleInEngine ee mod $ \em -> do
					fn <- getFunction em (mkName "main")
					case fn of
						Just f  -> run f
						Nothing -> return ()

				optmod <- M.moduleAST mod
				--BS.putStrLn =<< M.moduleLLVMAssembly mod
				return optmod 
	where
		optLevel = Just 0
		model    = Nothing
		ptrelim  = Nothing
		fastins  = Nothing



newtype CmpError
	= CmpError { getCmpError :: (L.AlexPosn, String) }
	deriving (Show)


data CmpState
	= CmpState
		{ llvmModule   :: Module
		, symTab       :: SymTab.SymTab Name Operand
		, instructions :: [Named Instruction]
		, uniqueCount  :: Word
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState)


initModule = defaultModule
	{ moduleName = BSS.toShort $ BS.pack "I just don't give a JIT"
	}


initCmpState = CmpState
	{ llvmModule   = initModule
	, symTab       = SymTab.initSymTab
	, instructions = []
	, uniqueCount  = 0
	}


unique :: Cmp Name
unique = do
	count <- gets uniqueCount
	modify $ \s -> s { uniqueCount = count + 1 }
	return $ UnName count


addDef :: Definition -> Cmp ()
addDef def =
	modify $ \s -> s { llvmModule = (llvmModule s)
		{ moduleDefinitions = (moduleDefinitions $ llvmModule s) ++ [def]
		}}


ensureDef :: Definition -> Cmp ()
ensureDef def = do
	defs <- gets (moduleDefinitions . llvmModule)
	unless (def `elem` defs) (addDef def)


defName :: Name -> Operand -> Cmp ()
defName name op =
	modify $ \s -> s { symTab = SymTab.insert name op (symTab s) }


lookupName :: L.AlexPosn -> String -> Cmp Operand
lookupName pos nameStr = do
	let name = mkName nameStr
	st <- gets symTab
	case SymTab.lookup name st of
		Nothing -> throwError $ CmpError (pos, nameStr ++ " doesn't exist")
		Just op -> return op


addInstr :: Named Instruction -> Cmp ()
addInstr instr =
	modify $ \s -> s { instructions = (instructions s) ++ [instr] }


cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Assign pos nameStr expr -> do
		let name = mkName nameStr

		st <- gets symTab
		case SymTab.lookup name [head st] of
			Just _  -> throwError $ CmpError (pos, nameStr ++ " already defined")	
			Nothing -> return ()

		val <- cmpExpr expr
		let typ = typeOf val

		defName name $ cons $ global (ptr typ) name

		if isCons val
		then
			let ConstantOperand cons = val in
			addDef $ globalVar name False typ (Just cons)
		else do
			addInstr $ Do $ Store False (cons $ global (ptr typ) name) val Nothing 0 []
			addDef $ globalVar name False typ (Just $ initOf typ)
	
	S.Set pos nameStr expr -> do
		st <- gets symTab
		op <- case SymTab.lookup (mkName nameStr) st of
			Nothing -> throwError $ CmpError (pos, nameStr ++ " doesn't exist")
			Just op -> return op

		val <- cmpExpr expr
		addInstr $ Do $ Store False op val Nothing 0 []
			

	S.Print pos exprs -> do
		vals <- mapM cmpExpr exprs
		let fmtStrs = (flip map) vals $ \val -> case typeOf val of
			IntegerType 32 -> "%d"

		let fmtStr = intercalate ", " fmtStrs
		fmt <- defString (fmtStr ++ "\n")

		ensureDef printfFn
		let printfTyp = FunctionType i32 [ptr i8] True
		let printfOp  = cons $ global (ptr printfTyp) (mkName "printf")
		let args      = (cons fmt, []) : [ (op, []) | op <- vals ]
		addInstr $ Do $ Call Nothing C [] (Right printfOp) args [] []


cmpExpr :: S.Expr -> Cmp Operand
cmpExpr expr = case expr of
	S.Int pos n ->
		return $ cons (C.Int 32 $ toInteger n)

	S.Ident pos idStr -> do
		op <- lookupName pos idStr		
		loadRef <- unique
		addInstr $ loadRef := Load False op Nothing 0 []
		return $ local (typeOf op) loadRef

	S.Infix pos op expr1 expr2 -> do
		val1 <- cmpExpr expr1
		val2 <- cmpExpr expr2

		if isCons val1 && isCons val2
		then return $ cons $ 
			let ConstantOperand cons1 = val1 in
			let ConstantOperand cons2 = val2 in
			case (typeOf (cons cons1), typeOf (cons cons2)) of
				(IntegerType 32, IntegerType 32) -> case op of
					S.Plus   -> C.Add False False cons1 cons2
					S.Minus  -> C.Sub False False cons1 cons2
					S.Times  -> C.Mul False False cons1 cons2
					S.Divide -> C.SDiv False cons1 cons2
					S.Mod    -> C.SRem cons1 cons2
		else do
			ref <- unique
			case (typeOf val1, typeOf val2) of
				(IntegerType 32, IntegerType 32) -> do
					ins <- case op of
						S.Plus   -> return $ Add False False val1 val2 []
						S.Minus  -> return $ Sub False False val1 val2 []
						S.Times  -> return $ Mul False False val1 val2 []
						S.Divide -> return $ SDiv False val1 val2 []
						S.Mod    -> return $ SRem val1 val2 []
						_ -> throwError $ CmpError (pos, "i32 does not support operator")
					addInstr (ref := ins)
					return (local i32 ref)


defString :: String -> Cmp C.Constant 
defString str = do
	let chars     = map (C.Int 8 . toInteger . ord) (str ++ "\0")
	let strArr    = C.Array i8 chars
	let strArrTyp = typeOf (cons strArr)

	strName <- unique
	addDef $ globalVar strName True strArrTyp (Just strArr)
	let strRef = global (ptr strArrTyp) strName
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


mainFn :: [Named Instruction] -> Definition
mainFn ins = GlobalDefinition $ functionDefaults
	{ G.returnType  = void
	, G.name        = mkName "main"
	, G.basicBlocks = [block]
	}
	where 
		block = BasicBlock (mkName "entry") ins (Do $ Ret Nothing [])


printfFn :: Definition
printfFn = GlobalDefinition $ functionDefaults
	{ G.returnType = i32
	, G.name       = mkName "printf"
	, G.parameters = ([Parameter (ptr i8) (mkName "fmt") []], True)
	}

