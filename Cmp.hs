{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}


module Cmp where

import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe

import           LLVM.AST 
import           LLVM.AST.Type              hiding (void)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified SymTab


class (MonadError CmpError m, MonadState (CmpState t) m, MonadModuleBuilder m) => MonadModuleCmp t m
class (MonadModuleCmp t m, MonadIRBuilder m) => MonadInstrCmp t m

instance Monad m => (MonadModuleCmp t) (ModuleCmpT t m)
instance Monad m => (MonadModuleCmp t) (InstrCmpT t m)
instance Monad m => (MonadInstrCmp t) (InstrCmpT t m)

instance MonadTrans (ModuleCmpT t) where
	--lift = (ModuleCmpT . ModuleBuilderT . StateT) (\s -> ExceptT)


type ModuleCmp t = ModuleCmpT t Identity
newtype ModuleCmpT t m a
	= ModuleCmpT { getModuleCmp :: ModuleBuilderT (StateT (CmpState t) (ExceptT CmpError m)) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState (CmpState t), MonadModuleBuilder)


type InstrCmp t = InstrCmpT t Identity
newtype InstrCmpT t m a
	= InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT t m) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState (CmpState t), MonadModuleBuilder, MonadIRBuilder) 


data CmpState t
	= CmpState
		{ symTab   :: SymTab.SymTab String (t, Operand)
		, externs  :: Map.Map String Definition
		, declared :: Set.Set String
		, exported :: Set.Set String
		}


data TextPos = TextPos { textPos, textLine, textCol :: Int } deriving (Show, Eq)


newtype CmpError
	= CmpError { getCmpError :: (TextPos, String) }
	deriving (Show)


runModuleCmpT
	:: Monad m
	=> ModuleBuilderState
	-> CmpState t
	-> ModuleCmpT t m a
	-> m (Either CmpError ((a, [Definition]), CmpState t))
runModuleCmpT moduleBuilderState cmpState moduleCmpT =
	runExceptT $
		(flip runStateT) cmpState $
			runModuleBuilderT moduleBuilderState $
				getModuleCmp moduleCmpT


runModuleCmp
	:: ModuleBuilderState
	-> CmpState t
	-> ModuleCmp t a
	-> Either CmpError ((a, [Definition]), CmpState t)
runModuleCmp moduleBuilderState cmpState moduleCmpT =
	runIdentity $
		runExceptT $
			(flip runStateT) cmpState $
				runModuleBuilderT moduleBuilderState $
					getModuleCmp moduleCmpT


initCmpState = CmpState
	{ symTab   = SymTab.initSymTab
	, externs  = Map.empty
	, declared = Set.empty
	, exported = Set.empty
	}


cmpErr:: MonadModuleCmp t m => TextPos -> String -> m a
cmpErr pos str =
	throwError $ CmpError (pos, str)


look :: MonadModuleCmp t m => TextPos -> String -> m (t, Operand)
look pos symbol = do
	(typ, addr) <- lookupSymbol pos symbol
	ext <- lookupExtern symbol

	when (isJust ext) $ do
		declared <- isDeclared symbol
		unless declared $ do
			emitDefn (fromJust ext)
			addDeclared symbol

	return (typ, addr)


ensureExtern :: MonadModuleCmp t m => String -> [Type] -> Type -> Bool -> m Operand
ensureExtern name ats rt varg = do
	let dotName = '.' : name

	declared <- isDeclared dotName
	unless declared $ do
		addDeclared dotName
		void $ (if varg then externVarArgs else extern) (mkName name) ats rt

	let typ = FunctionType rt ats varg
	return $ ConstantOperand $ C.GlobalReference (ptr typ) (mkName name)


checkSymbolUndefined :: MonadModuleCmp t m => TextPos -> String -> m ()
checkSymbolUndefined pos symbol = do
	st <- fmap head (gets symTab)
	unless (isNothing $ SymTab.lookup symbol [st]) $ cmpErr pos (symbol ++ " already defined") 


addDeclared :: MonadModuleCmp t m => String -> m ()
addDeclared symbol =
	modify $ \s -> s { declared = Set.insert symbol (declared s) }


addExported :: MonadModuleCmp t m => String -> m ()
addExported symbol =
	modify $ \s -> s { exported = Set.insert symbol (exported s) }


addExtern :: MonadModuleCmp t m => String -> Definition -> m ()
addExtern symbol def =
	modify $ \s -> s { externs = Map.insert symbol def (externs s) }


addSymbol :: MonadModuleCmp t m => String -> (t, Operand) -> m ()
addSymbol symbol val =
	modify $ \s -> s { symTab = SymTab.insert symbol val (symTab s) }


isDeclared :: MonadModuleCmp t m => String -> m Bool
isDeclared symbol =
	fmap (elem symbol) (gets declared)


lookupExtern :: MonadModuleCmp t m => String -> m (Maybe Definition)
lookupExtern symbol =
	fmap (Map.lookup symbol) (gets externs)


lookupSymbol :: MonadModuleCmp t m => TextPos -> String -> m (t, Operand)
lookupSymbol pos symbol = do
	st <- gets symTab
	maybe (cmpErr pos $ symbol ++ " doesn't exist") return (SymTab.lookup symbol st)


pushSymTab :: MonadModuleCmp t m => m ()
pushSymTab =
	modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: MonadModuleCmp t m => m ()
popSymTab =
	modify $ \s -> s { symTab = SymTab.pop (symTab s) }


