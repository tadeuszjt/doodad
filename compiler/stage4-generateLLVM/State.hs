{-# LANGUAGE FlexibleContexts #-}
module State where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)

import qualified LLVM.AST.Constant as C
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.AST as LL
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C hiding (type')
import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.Context
import Foreign.Ptr

import qualified AST as S
import Type
import qualified SymTab
import qualified JIT
import Monad
import Error
import Trace
import Symbol


data Value2 
    = Value2 Type LL.Operand
    deriving (Show, Eq)

data Pointer
    = Pointer Type LL.Operand
    deriving (Show, Eq)


loc :: Pointer -> LL.Operand
loc (Pointer _ l) = l


op :: Value2 -> LL.Operand
op (Value2 _ o) = o


data Value
    = Val Type LL.Operand
    | Ptr Type LL.Operand
    | ConstInt Integer
    deriving (Show, Eq)

isPtr :: Value -> Bool
isPtr (Ptr _ _) = True
isPtr _         = False

valOp :: Value -> LL.Operand
valOp (Val _ op) = op
valOp (Ptr typ _) = error (show typ)


valLoc :: Value -> LL.Operand
valLoc (Ptr _ loc) = loc
valLoc (Val typ _) = error (show typ)


data Object
    = ObjFn         
    | ObjVal Pointer
    | ObjType Type
    | ObjField Int
    deriving ()


instance Show Object where
    show object = case object of
        ObjVal (Pointer t o) -> "Ptr " ++ show t
        ObjVal val       -> "val"
        ObjType typ      -> show typ
        ObjFn            -> "fn"
        ObjField i       -> "." ++ show i


data CompileState
    = CompileState
        { context     :: Context
        , dataLayout  :: Ptr FFI.DataLayout
        , declared    :: Set.Set LL.Name
        , objects     :: Map.Map Symbol Object
        , moduleName  :: String
        , nameSupply  :: Map.Map String Int
        , stringMap   :: Map.Map String C.Constant
        , typeNameMap :: Map.Map Type LL.Name
        }

initCompileState modName session 
     = CompileState
        { context     = JIT.context session
        , dataLayout  = JIT.dataLayout session
        , declared    = Set.empty
        , objects     = Map.empty
        , moduleName  = modName
        , nameSupply  = Map.empty
        , stringMap   = Map.empty
        , typeNameMap = Map.empty
        }


mkBSS = BSS.toShort . BS.pack


mkNameFromSymbol :: Symbol -> LL.Name 
mkNameFromSymbol (SymResolved mod sym 0) = LL.mkName $ mod ++ "." ++ sym
mkNameFromSymbol (SymResolved mod sym n) = LL.mkName $ mod ++ "." ++ sym ++ "_" ++ show n


fnSymbolToName :: Symbol -> LL.Name
fnSymbolToName (SymResolved mod sym 0)     = LL.mkName $ mod ++ "." ++ sym
fnSymbolToName (SymResolved mod sym level) = LL.mkName $ mod ++ "." ++ sym ++ "_" ++ show level
fnSymbolToName (SymQualified "c" sym)      = LL.mkName $ sym 


getStringPointer :: InsCmp CompileState m => String -> m LL.Operand
getStringPointer str = do
    resm <- Map.lookup str <$> gets stringMap
    case resm of
        Just loc -> return (LL.ConstantOperand loc)
        Nothing  -> do
            p <- globalStringPtr str =<< myFresh "str"
            modify $ \s -> s { stringMap = Map.insert str p (stringMap s) }
            return (LL.ConstantOperand p)


myFresh :: InsCmp CompileState m => String -> m LL.Name
myFresh sym = do
    nameSupply <- gets nameSupply
    mod <- gets moduleName
    let n = maybe 0 (+1) (Map.lookup sym nameSupply)
    modify $ \s -> s { nameSupply = Map.insert sym n nameSupply }
    return $ LL.mkName $ case n of
        0 -> mod ++ "." ++ sym
        _ -> mod ++ "." ++ sym ++ "_" ++ show n


define :: BoM CompileState m => Symbol -> Object -> m ()
define symbol obj = trace "define" $ do
    res <- Map.lookup symbol <$> gets objects
    assert (isNothing res) (show symbol ++ " already defined")
    modify $ \s -> s { objects = Map.insert symbol obj (objects s) }


ensureExtern :: ModCmp CompileState m => LL.Name -> [LL.Type] -> LL.Type -> Bool -> m LL.Operand
ensureExtern name argTypes retty isVarg = trace "ensureExtern" $ do
    isDeclared <- Set.member name <$> gets declared
    when (not isDeclared) $ do
        case isVarg of
            True -> externVarArgs name argTypes retty
            False -> extern name argTypes retty
        modify $ \s -> s { declared = Set.insert name (declared s) }
    return $ LL.ConstantOperand $
        C.GlobalReference (LL.ptr $ LL.FunctionType retty argTypes isVarg) name


lookm :: ModCmp CompileState m => Symbol -> m (Maybe Object)
lookm symbol = Map.lookup symbol <$> gets objects


look :: ModCmp CompileState m => Symbol -> m Object
look symbol = do
    resm <- lookm symbol
    assert (isJust resm) $ "no definition for: " ++ show symbol
    return (fromJust resm)

