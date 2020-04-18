{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmpState where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import Control.Monad.Fail
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Char

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.CallingConvention
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C

import qualified SymTab


type Extern = (Name, Definition)


data TextPos
	= TextPos
		{ textPos  :: Int
		, textLine :: Int
		, textCol  :: Int
		}
	deriving (Show, Eq)


newtype CmpError
	= CmpError { getCmpError :: (TextPos, String) }
	deriving (Show)


data CmpState
	= CmpState
		{ symTab       :: SymTab.SymTab String (Operand, Maybe Extern)
		, nameSupply   :: Map.Map String Int
		, uniqueCount  :: Word
		, declared     :: Set.Set Name
		, exported     :: Set.Set Name
		, globals      :: [Definition]
		, basicBlocks  :: [[BasicBlock]]
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
	, basicBlocks  = [[]]
	}


cmpErr :: TextPos -> String -> Cmp a
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


addSymbol :: String -> Operand -> Maybe Extern -> Cmp ()
addSymbol name op extern =
	modify $ \s -> s { symTab = SymTab.insert name (op, extern) (symTab s) }


lookupSymbol :: TextPos -> String -> Cmp Operand
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


pushSymTab :: Cmp ()
pushSymTab =
	modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Cmp ()
popSymTab =
	modify $ \s -> s { symTab = SymTab.pop (symTab s) }


checkSymbolIsFree :: TextPos -> String -> Cmp ()
checkSymbolIsFree pos name =
	gets symTab >>= \st -> case SymTab.lookup name [head st] of
		Just _  -> cmpErr pos (name ++ " already defined")
		Nothing -> return ()


pushBlocks :: Cmp ()
pushBlocks =
	modify $ \s -> s { basicBlocks = [] : (basicBlocks s) }


popBlocks :: Cmp [BasicBlock]
popBlocks = do
	blocks <- gets basicBlocks
	modify $ \s -> s { basicBlocks = tail blocks }
	return $ reverse (head blocks)


addBlock :: BasicBlock -> Cmp ()
addBlock block = do
	blocks <- gets basicBlocks
	modify $ \s -> s { basicBlocks = (head blocks ++ [block]) : (tail blocks) }


instr :: Named Instruction -> Cmp ()
instr ins = do
	blocks <- gets basicBlocks
	let BasicBlock name instructions terminator = head (head blocks)
	let newBlock = BasicBlock name (instructions ++ [ins]) terminator 
	modify $ \s -> s { basicBlocks = (newBlock : tail (head blocks)) : (tail blocks) }

