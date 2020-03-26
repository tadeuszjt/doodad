{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import qualified Data.Map as Map
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified AST   as S
import qualified Lexer as L
import qualified SymTab

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.Constant as C
import LLVM.AST.Linkage
import LLVM.AST.AddrSpace
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention

newtype CmpError
	= CmpError { getCmpError :: (L.AlexPosn, String) }
	deriving (Show)



newtype LLVM a
	= LLVM { getLLVM :: ExceptT CmpError (State Module) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState Module)


data CmpState
	= CmpState
		{ modul  :: Module
		, symTab :: SymTab.SymTab Name Operand
		}


initModule = defaultModule
	{ moduleName = BSS.toShort $ BS.pack "I just don't give a JIT"
	}


initCmpState = CmpState
	{ modul  = initModule
	, symTab = SymTab.initSymTab
	}

codeGen :: CmpState -> S.AST -> IO (Either CmpError CmpState)
codeGen cmpState ast = do
	putStrLn "benis"
	return $ Right cmpState


--compileAST :: S.AST -> Either CmpError Module
--compileAST ast =
--	evalState (runExceptT $ getCmp $ cmpAST ast) initCmpState 
--
--
--data BlockState
--	= BlockState
--		{ instructions :: [Named Instruction]
--		, terminator   :: Maybe (Named Terminator)
--		}
--	deriving Show
--
--
--data FnState
--	= FnState
--		{ uniqueCount :: Int
--		, blocks      :: Map.Map Name BlockState
--		, curBlock    :: Name
--		, retType     :: Type
--		, fnArgs      :: [(Name, Type)]
--		}
--	deriving (Show)
--
--
--data CmpState
--	= CmpState
--		{ functions :: Map.Map Name FnState
--		, currentFn :: Name
--		, symTab    :: SymTab.SymTab Name Operand
--		}
--	deriving (Show)
--
--
--newtype Cmp a
--	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
--	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState)
--
--
--initBlockState = BlockState
--	{ instructions = []
--	, terminator   = Nothing
--	}
--
--
--initFnState = FnState
--	{ uniqueCount = 0
--	, blocks      = Map.singleton (mkName "entry") initBlockState
--	, curBlock    = mkName "entry"
--	, retType     = void 
--	, fnArgs      = []
--	}
--
--
--initCmpState = CmpState
--	{ functions = Map.empty 
--	, currentFn = mkName ""
--	, symTab    = SymTab.initSymTab
--	}
--
--
--compileAST :: S.AST -> Either CmpError Module
--compileAST ast =
--	evalState (runExceptT $ getCmp $ cmpAST ast) initCmpState 
--
--
--cmpAST :: S.AST -> Cmp Module
--cmpAST ast = do
--	mapM_ cmpTopStmt ast
--	fns <- fmap Map.toList (gets functions)
--
--	return $ defaultModule
--		{ moduleDefinitions = map fnDef fns
--		}
--
--	where
--		fnDef :: (Name, FnState) -> Definition
--		fnDef (name, fn) = GlobalDefinition $ functionDefaults
--			{ name        = name
--			, returnType  = retType fn
--			, parameters  = ([Parameter ty nm [] | (nm, ty) <- fnArgs fn], False)
--			, basicBlocks = map mkBlock (Map.toList $ blocks fn)
--			}
--
--
--		mkBlock :: (Name, BlockState) -> BasicBlock
--		mkBlock (name, blk) =
--			case terminator blk of
--				Nothing -> error $ "block has no terminator: " ++ show (name, blk)
--				Just t  -> BasicBlock name (instructions blk) t
--
--
--modifyCurFn :: (FnState -> FnState) -> Cmp ()
--modifyCurFn f = do
--	curFn <- gets currentFn
--	fn <- fmap (Map.! curFn) (gets functions)
--	modify $ \s -> s { functions = Map.insert curFn (f fn) (functions s) }
--
--
--modifyCurBlock :: (BlockState -> BlockState) -> Cmp ()
--modifyCurBlock f =
--	modifyCurFn $ \fn -> let
--			name = curBlock fn
--			blks = blocks fn
--			blk  = (Map.!) blks name
--		in fn { blocks = Map.insert name (f blk) blks }
--
--
--instr :: Named Instruction -> Cmp ()
--instr ins =
--	modifyCurBlock $ \blk -> blk { instructions = instructions blk ++ [ins] }
--
--
--term :: Named Terminator -> Cmp ()
--term ter =
--	modifyCurBlock $ \blk -> blk { terminator = Just ter }
--
--
--cmpTopStmt :: S.Stmt -> Cmp ()
--cmpTopStmt stmt = case stmt of
--	S.Assign pos name expr -> do
--		return ()
--
--	S.Print pos exprs -> do
--		return ()
--
