{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import qualified Data.Map as Map
import Data.Char
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab

import LLVM.AST
import LLVM.AST.Type
import qualified LLVM.AST.Global as LG
import LLVM.AST.Name
import LLVM.AST.Instruction
import qualified LLVM.AST.Constant as C
import LLVM.AST.Linkage
import LLVM.AST.AddrSpace
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention



codeGen :: CmpState -> S.AST -> IO (Either CmpError CmpState)
codeGen cmpState ast = do
	let (res, cmpState') = runState (runExceptT $ getCmp $ mapM_ cmpTopStmt ast) cmpState
	case res of
		Left cmpErr -> return (Left cmpErr)
		Right _     -> return (Right cmpState')



newtype CmpError
	= CmpError { getCmpError :: (L.AlexPosn, String) }
	deriving (Show)


newtype LLVM a
	= LLVM { getLLVM :: ExceptT CmpError (State Module) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState Module)


data CmpState
	= CmpState
		{ llvmModule  :: Module
		, symTab      :: SymTab.SymTab Name Operand
		, uniqueCount :: Word
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState)


initModule = defaultModule
	{ moduleName = BSS.toShort $ BS.pack "I just don't give a JIT"
	}


initCmpState = CmpState
	{ llvmModule  = initModule
	, symTab      = SymTab.initSymTab
	, uniqueCount = 0
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

				let loadRef = UnName 0
				let load    = loadRef := Load False op Nothing 0 []

				let storeAddr = cons $ global (ptr opTyp) name
				let storeVal  = local opTyp loadRef
				let store     = Do $ Store False storeAddr storeVal Nothing 0 []

				addDef (mainFn [load, store])
				return (initOf opTyp, opTyp)
			
		addDef $ globalVar name False typ (Just init)
		defName name $ cons (global (ptr typ) name)

	S.Print pos [expr] -> do
		fmt <- defString "benis\n"

		ensureDef printfFn
		let printfTyp = FunctionType i32 [ptr i8] True
		let printfOp  = cons $ global (ptr printfTyp) (mkName "printf")
		let args      = [(cons fmt, [])]
		let ins       = Do $ Call Nothing C [] (Right printfOp) args [] []
		addDef (mainFn [ins])
		return ()


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
typeOf (ConstantOperand (C.Int 32 _))                             = i32
typeOf (ConstantOperand (C.GlobalReference (PointerType typ _)_)) = typ
typeOf (ConstantOperand (C.Array t elems))                      
	= ArrayType (fromIntegral $ length elems) t
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
	{ LG.name        = name
	, LG.isConstant  = isCons
	, LG.type'       = typ
	, LG.initializer = init
	}


mainFn :: [Named Instruction] -> Definition
mainFn ins = GlobalDefinition $ functionDefaults
	{ LG.returnType  = void
	, LG.name        = mkName "main"
	, LG.basicBlocks = [block]
	}
	where 
		block = BasicBlock (mkName "entry") ins (Do $ Ret Nothing [])


printfFn :: Definition
printfFn = GlobalDefinition $ functionDefaults
	{ LG.returnType = i32
	, LG.name       = mkName "printf"
	, LG.parameters = ([Parameter (ptr i8) (mkName "fmt") []], True)
	}

