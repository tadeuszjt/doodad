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
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module hiding (global)
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
		, curRetType   :: Type
		, declared     :: Set.Set Name
		, exported     :: Set.Set Name
		, basicBlocks  :: [[BasicBlock]]
		}


newtype Cmp a
	= Cmp { getCmp :: ExceptT CmpError (ModuleBuilderT (State CmpState)) a }
	deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState CmpState, MonadModuleBuilder)


initCmpState = CmpState
	{ symTab       = SymTab.initSymTab
	, nameSupply   = Map.empty
	, uniqueCount  = 0
	, curRetType   = VoidType
	, declared     = Set.empty
	, exported     = Set.empty
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


reserveName :: String -> Cmp ()
reserveName name = do
	supply <- gets nameSupply
	unless (isJust $ Map.lookup name supply) (void $ uniqueName name)


addDeclared :: Name -> Cmp ()
addDeclared name =
	modify $ \s -> s { declared = Set.insert name (declared s) }
	  

addExported :: Name -> Cmp ()
addExported name =
	modify $ \s -> s { exported = Set.insert name (exported s) }


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
					unless (nm `elem` decls) (emitDefn def >> addDeclared nm)
			return op


pushSymTab :: Cmp ()
pushSymTab =
	modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Cmp ()
popSymTab =
	modify $ \s -> s { symTab = SymTab.pop (symTab s) }


checkSymbolUndefined :: TextPos -> String -> Cmp ()
checkSymbolUndefined pos name =
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


addBlock :: Name -> Cmp ()
addBlock name = do
	blocks <- gets basicBlocks
	let block = BasicBlock name [] (Do $ Ret Nothing [])
	modify $ \s -> s { basicBlocks = (block : head blocks) : (tail blocks) }


instr :: Named Instruction -> Cmp ()
instr ins = do
	blocks <- gets basicBlocks
	let BasicBlock name instructions terminator = head (head blocks)
	let newBlock = BasicBlock name (instructions ++ [ins]) terminator 
	modify $ \s -> s { basicBlocks = (newBlock : tail (head blocks)) : (tail blocks) }


terminator :: Named Terminator -> Cmp ()
terminator term = do
	blocks <- gets basicBlocks
	let BasicBlock name instructions _ = head (head blocks)
	let newBlock = BasicBlock name instructions term 
	modify $ \s -> s { basicBlocks = (newBlock : tail (head blocks)) : (tail blocks) }


typeOf :: Operand -> Type
typeOf (LocalReference (PointerType typ _) _) = typ
typeOf (LocalReference typ _) = typ
typeOf (ConstantOperand cons) = case cons of
	C.GlobalReference (PointerType typ _) _ -> typ
	C.Int nb _                              -> IntegerType nb
	C.Array typ elems                       -> ArrayType (fromIntegral $ length elems) typ
	_                                       -> error (show cons)
typeOf x = error (show x)


globalVar :: Type -> Name -> Bool -> Maybe C.Constant -> Definition
globalVar typ name isCons init =
	GlobalDefinition $ globalVariableDefaults
		{ G.name        = name
		, G.isConstant  = isCons
		, G.type'       = typ
		, G.initializer = init
		}


funcDef :: Name -> Type -> [(Name, Type)] -> Bool -> [BasicBlock] -> Definition
funcDef name retType args isVarg basicBlocks =
	GlobalDefinition $ functionDefaults
		{ G.returnType  = retType
		, G.name        = name
		, G.parameters  = ([ Parameter t n [] | (n, t) <- args ], isVarg)
		, G.basicBlocks = basicBlocks
		}


string :: String -> Cmp Operand
string str = do
	let array = C.Array i8 $ map (C.Int 8 . toInteger . ord) (str ++ "\0")
	let typ = typeOf (cons array)

	name <- uniqueName "string"
	emitDefn $ globalVar typ name True (Just array)
	return $ global (ptr typ) name


initOf :: Type -> C.Constant
initOf typ = case typ of
	IntegerType nbits -> C.Int nbits 0


isCons :: Operand -> Bool
isCons (ConstantOperand _) = True
isCons _                   = False


isPtr :: Operand -> Bool
isPtr (ConstantOperand (C.GlobalReference (PointerType _ _) _)) = True
isPtr (LocalReference (PointerType _ _) _)                      = True
isPtr _                                                         = False


toCons :: Operand -> C.Constant
toCons (ConstantOperand c) = c


global :: Type -> Name -> Operand
global typ name = cons (C.GlobalReference typ name)


local :: Type -> Name -> Operand
local = LocalReference


cons :: C.Constant -> Operand
cons = ConstantOperand


add :: Operand -> Operand -> Cmp Operand
add a b = do
	un <- unique
	instr $ un := Add False False a b []
	return $ local (typeOf a) un


sub :: Operand -> Operand -> Cmp Operand
sub a b = do
	un <- unique
	instr $ un := Sub False False a b []
	return $ local (typeOf a) un


mul :: Operand -> Operand -> Cmp Operand
mul a b = do
	unless (typeOf a == typeOf b) (error "int types don't match")
	un <- unique
	instr $ un := Mul False False a b []
	return $ local (typeOf a) un


idiv :: Operand -> Operand -> Cmp Operand
idiv a b = do
	un <- unique
	instr $ un := SDiv False a b []
	return $ local (typeOf a) un


imod :: Operand -> Operand -> Cmp Operand
imod a b = do
	un <- unique
	instr $ un := SRem a b []
	return $ local (typeOf a) un


icmp :: Operand -> Operand -> IntegerPredicate -> Cmp Operand
icmp a b ip = do
	unless (typeOf a == typeOf b) (error "int types don't match")
	un <- unique
	instr $ un := ICmp ip a b []
	return $ local (ptr i1) un


store :: Operand -> Operand -> Cmp ()
store addr val =
	instr $ Do $ Store False addr val Nothing 0 []


load :: Operand -> Cmp Operand
load addr = do
	un <- unique
	instr $ un := Load False addr Nothing 0 []
	return $ local (typeOf addr) un


zext :: Type -> Operand -> Cmp Operand
zext typ val = do
	un <- unique
	instr $ un := ZExt val typ []
	return $ local typ un


bitcast :: Type -> Operand -> Cmp Operand
bitcast typ val = do
	un <- unique
	instr $ un := BitCast val typ []
	return $ local typ un
	

select :: Operand -> Operand -> Operand -> Cmp Operand
select s a b = do
	unless (typeOf a == typeOf b) (error "types don't match")
	un <- unique
	instr $ un := Select s a b []
	return $ local (typeOf a) un


call :: Operand -> [Operand] -> Cmp (Maybe Operand)
call op args =
	let typ = typeOf op in
	case resultType typ of
		VoidType -> do
			instr $ Do $ Call Nothing C [] (Right op) [(arg, []) | arg <- args] [] []
			return Nothing
		retType  -> do
			un <- unique
			instr $ un := Call Nothing C [] (Right op) [(arg, []) | arg <- args] [] []
			return $ Just $ local retType un


alloca :: Type -> Name -> Cmp ()
alloca typ name =
	instr $ name := Alloca typ Nothing 0 []


cndBr :: Operand -> Name -> Name -> Named Terminator
cndBr cnd trueDest falseDest =
	Do $ CondBr cnd trueDest falseDest []


brk :: Name -> Named Terminator
brk dest =
	Do $ Br dest []


subscript :: Operand -> Operand -> Cmp Operand
subscript arr idx = do
	let typ@(ArrayType _ elemType) = typeOf arr
	elem <- unique
	instr $ elem := GetElementPtr True arr [(cons $ C.Int 8 0), idx] []
	bitcast (ptr elemType) $ local typ elem

