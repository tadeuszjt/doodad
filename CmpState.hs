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

import LLVM.AST
import LLVM.AST.Type hiding (void)
import LLVM.AST.Name
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.CallingConvention
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C

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


data CmpState
	= CmpState
		{ symTab       :: SymTab.SymTab String (Operand, Maybe Definition)
		, declared     :: Set.Set String
		, curRetType   :: Type
		, exported     :: Set.Set Name
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (State CmpState) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState, MonadFix)


initCmpState = CmpState
	{ symTab       = SymTab.initSymTab
	, declared     = Set.empty
	, curRetType   = VoidType
	, exported     = Set.empty
	}


cmpErr :: TextPos -> String -> Cmp a
cmpErr pos str = throwError $ CmpError (pos, str)


addDeclared :: String -> Cmp ()
addDeclared symbol =
	modify $ \s -> s { declared = Set.insert symbol (declared s) }
	  

isDeclared :: String -> Cmp Bool
isDeclared symbol =
	fmap (elem symbol) (gets declared)


addExported :: Name -> Cmp ()
addExported name =
	modify $ \s -> s { exported = Set.insert name (exported s) }


addSymbol :: String -> Operand -> Maybe Definition -> Cmp ()
addSymbol name op extern =
	modify $ \s -> s { symTab = SymTab.insert name (op, extern) (symTab s) }


lookupSymbol :: TextPos -> String -> Cmp (Operand, Maybe Definition)
lookupSymbol pos name = do
	st <- gets symTab
	maybe (cmpErr pos $ name ++ " doesn't exist") return (SymTab.lookup name st)


pushSymTab :: Cmp ()
pushSymTab =
	modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Cmp ()
popSymTab =
	modify $ \s -> s { symTab = SymTab.pop (symTab s) }


setCurRetType :: Type -> Cmp ()
setCurRetType typ =
	modify $ \s -> s { curRetType = typ }


getCurRetType :: Cmp Type
getCurRetType =
	gets curRetType


checkSymbolUndefined :: TextPos -> String -> Cmp ()
checkSymbolUndefined pos name =
	gets symTab >>= \st -> case SymTab.lookup name [head st] of
		Just _  -> cmpErr pos (name ++ " already defined")
		Nothing -> return ()


initOf :: Type -> C.Constant
initOf typ = case typ of
	IntegerType nbits -> C.Int nbits 0
	ArrayType n t     -> C.Array t (replicate (fromIntegral n) (initOf t))
	_                 -> error (show typ)


isCons :: Operand -> Bool
isCons (ConstantOperand _) = True
isCons _                   = False


toCons :: Operand -> C.Constant
toCons (ConstantOperand c) = c


cons :: C.Constant -> Operand
cons = ConstantOperand


globalDef :: Name -> Type -> Maybe C.Constant -> Definition
globalDef nm ty init = GlobalDefinition globalVariableDefaults
	{ G.name        = nm
	, G.type'       = ty
	, G.initializer = init
	}


funcDef :: Name -> [(Type, Name)] -> Type -> [BasicBlock] -> Definition
funcDef nm params retty blocks = GlobalDefinition functionDefaults
	{ G.name        = nm
	, G.parameters  = ([Parameter t n [] | (t, n) <- params], False)
	, G.returnType  = retty
	, G.basicBlocks = blocks
	}
