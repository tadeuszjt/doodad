{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmpState where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)
import Control.Monad.Fail
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Maybe

import LLVM.AST.Type hiding (void)

import qualified SymTab


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


data CmpState o d
	= CmpState
		{ symTab       :: SymTab.SymTab String o
		, externs      :: Map.Map String d
		, declared     :: Set.Set String
		, exported     :: Set.Set String
		, curRetType   :: Type
		}


newtype Cmp o d a
	= Cmp { getCmp :: ExceptT CmpError (State (CmpState o d)) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState (CmpState o d), MonadFix)


initCmpState = CmpState
	{ symTab       = SymTab.initSymTab
	, externs      = Map.empty
	, declared     = Set.empty
	, exported     = Set.empty
	, curRetType   = VoidType
	}


cmpErr :: TextPos -> String -> Cmp o d a
cmpErr pos str = throwError $ CmpError (pos, str)


addDeclared :: String -> Cmp o d ()
addDeclared symbol =
	modify $ \s -> s { declared = Set.insert symbol (declared s) }
	  

isDeclared :: String -> Cmp o d Bool
isDeclared symbol =
	fmap (elem symbol) (gets declared)


addExported :: String -> Cmp o d ()
addExported symbol =
	modify $ \s -> s { exported = Set.insert symbol (exported s) }


addExtern :: String -> d -> Cmp o d ()
addExtern symbol def =
	modify $ \s -> s { externs = Map.insert symbol def (externs s) }


lookupExtern :: String -> Cmp o d (Maybe d)
lookupExtern symbol =
	fmap (Map.lookup symbol) (gets externs)


addSymbol :: String -> o -> Cmp o d ()
addSymbol symbol op =
	modify $ \s -> s { symTab = SymTab.insert symbol op (symTab s) }


lookupSymbol :: TextPos -> String -> Cmp o d o
lookupSymbol pos symbol = do
	st <- gets symTab
	maybe (cmpErr pos $ symbol ++ " doesn't exist") return (SymTab.lookup symbol st)


pushSymTab :: Cmp o d ()
pushSymTab =
	modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Cmp o d ()
popSymTab =
	modify $ \s -> s { symTab = SymTab.pop (symTab s) }


setCurRetType :: Type -> Cmp o d ()
setCurRetType typ =
	modify $ \s -> s { curRetType = typ }


getCurRetType :: Cmp o d Type
getCurRetType =
	gets curRetType


checkSymbolUndefined :: TextPos -> String -> Cmp o d ()
checkSymbolUndefined pos symbol =
	gets symTab >>= \st -> case SymTab.lookup symbol [head st] of
		Just _  -> cmpErr pos (symbol ++ " already defined")
		Nothing -> return ()
