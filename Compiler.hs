{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Data.Char
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


jit :: Context -> (MCJIT -> IO a) -> IO a
jit ctx f = withMCJIT ctx optLevel model ptrelim fastins f
	where
		optLevel = Just 0
		model    = Nothing
		ptrelim  = Nothing
		fastins  = Nothing


passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }


runJIT :: Module -> IO Module
runJIT mod =
	withContext (\ctx -> jit ctx $ mcJITAction ctx)
	where
		mcJITAction :: Context -> MCJIT -> IO Module
		mcJITAction context ee = 
			M.withModuleFromAST context mod (moduleAction ee)

		moduleAction :: MCJIT -> M.Module -> IO Module
		moduleAction ee m = do
			s <- M.moduleLLVMAssembly m
			BS.putStrLn s

			withModuleInEngine ee m $ \em -> do
				mainfn <- getFunction em (mkName "main")
				case mainfn of
					Just fn -> run fn
					Nothing -> return ()

			M.moduleAST m 


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


addInstr :: Named Instruction -> Cmp ()
addInstr instr =
	modify $ \s -> s { instructions = (instructions s) ++ [instr] }


ensureDef :: Definition -> Cmp ()
ensureDef def = do
	defs <- gets (moduleDefinitions . llvmModule)
	unless (def `elem` defs) (addDef def)


defName :: Name -> Operand -> Cmp ()
defName name op =
	modify $ \s -> s { symTab = SymTab.insert name op (symTab s) }


lookupName :: String -> L.AlexPosn -> Cmp Operand
lookupName nameStr pos = do
	let name = mkName nameStr
	st <- gets symTab
	case SymTab.lookup name st of
		Nothing -> throwError $ CmpError (pos, nameStr ++ " doesn't exist")
		Just op -> return op




cmpTopStmt :: S.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	S.Assign pos nameStr expr -> do
		let name = mkName nameStr

		st <- gets symTab
		case SymTab.lookup name [head st] of
			Just _  -> throwError $ CmpError (pos, nameStr ++ " already defined")	
			Nothing -> return ()

		(init, typ) <- case expr of
			S.Int _ n   ->
				return (C.Int 32 (toInteger n), i32)

			S.Ident _ idStr -> do
				op <- lookupName idStr pos
				let opTyp = typeOf op

				loadRef <- unique
				let load = loadRef := Load False op Nothing 0 []

				let storeAddr = cons $ global (ptr opTyp) name
				let storeVal  = local opTyp loadRef
				let store     = Do $ Store False storeAddr storeVal Nothing 0 []

				addInstr load
				addInstr store
				return (initOf opTyp, opTyp)
			
		addDef $ globalVar name False typ (Just init)
		defName name $ cons (global (ptr typ) name)

	S.Print pos [expr] -> do
		fmt <- defString "benis\n"

		ensureDef printfFn
		let printfTyp = FunctionType i32 [ptr i8] True
		let printfOp  = cons $ global (ptr printfTyp) (mkName "printf")
		let args      = [(cons fmt, [])]
		addInstr $ Do $ Call Nothing C [] (Right printfOp) args [] []


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
typeOf (ConstantOperand cons) = case cons of
	C.Int 32 _                              -> i32
	C.GlobalReference (PointerType typ _) _ -> typ
	C.Array t elems                         -> ArrayType (fromIntegral $ length elems) t
typeOf x = error $ show x


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

