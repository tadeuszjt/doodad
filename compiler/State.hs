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


valType :: Value -> Type
valType (Ptr typ _) = typ
valType (Val typ _) = typ


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [Type] [Type] Type
    | KeyField Type
    | KeyTypeField Type
    deriving (Eq, Ord)

instance Show SymKey where
    show KeyType = "type"
    show KeyVar  = "var"
    show (KeyFunc ps ts rt) = "fn" ++ S.brcStrs (map show ps) ++ " (" ++ intercalate ", " (map show ts) ++ ")" ++ show rt
    show (KeyField t) = show t ++ "."
    show (KeyTypeField typ) = show typ


data Object
    = ObjVal          Value
    | ObType          Type
    | ObjFn         
    | ObjAdtTypeField Int
    | ObjConstructor 
    | ObjField       Int
    deriving ()


instance Show Object where
    show object = case object of
        ObjVal (Val t o)    -> "Val " ++ show t
        ObjVal (Ptr t o)    -> "Ptr " ++ show t
        ObjVal val          -> "val"
        ObType typ          -> show typ
        ObjFn             -> "fn"
        ObjConstructor      -> "(..)"
        ObjField i         -> "." ++ show i


data Declaration
    = DecExtern [LL.Type] LL.Type Bool
    | DecVar    LL.Type
    deriving (Show)


data CompileState
    = CompileState
        { imports       :: [CompileState]
        , decMap        :: Map.Map (Symbol, SymKey) LL.Name
        , declarations  :: Map.Map LL.Name Declaration
        , declared      :: Set.Set LL.Name
        , symTab        :: SymTab.SymTab Symbol SymKey Object
        , curModName    :: String
        , nameMap       :: Map.Map String Int
        , stringMap     :: Map.Map String C.Constant
        }

initCompileState imports modName
     = CompileState
        { imports       = imports
        , decMap        = Map.empty
        , declarations  = Map.empty
        , declared      = Set.empty
        , symTab        = SymTab.initSymTab
        , curModName    = modName
        , nameMap       = Map.empty
        , stringMap     = Map.empty
        }


mkBSS = BSS.toShort . BS.pack



getStringPointer :: InsCmp CompileState m => String -> m LL.Operand
getStringPointer str = do
    resm <- Map.lookup str <$> gets stringMap
    case resm of
        Just loc -> return (LL.ConstantOperand loc)
        Nothing  -> do
            p <- globalStringPtr str =<< myFreshPrivate "str"
            modify $ \s -> s { stringMap = Map.insert str p (stringMap s) }
            return (LL.ConstantOperand p)


myFreshPrivate :: InsCmp CompileState m => String -> m LL.Name
myFreshPrivate sym = do
    nameMap <- gets nameMap
    let n = maybe 0 (+1) (Map.lookup sym nameMap)
    modify $ \s -> s { nameMap = Map.insert sym n nameMap }
    case n of
        0 -> return $ LL.Name $ mkBSS ("." ++ sym)
        _ -> return $ LL.Name $ mkBSS ("." ++ sym ++ "." ++ show n)

myFresh :: InsCmp CompileState m => String -> m LL.Name
myFresh sym = do
    nameMap <- gets nameMap
    mod <- gets curModName
    let n = maybe 0 (+1) (Map.lookup sym nameMap)
    modify $ \s -> s { nameMap = Map.insert sym n nameMap }
    case n of
        0 -> return $ LL.Name $ mkBSS (mod ++ "." ++ sym)
        _ -> return $ LL.Name $ mkBSS (mod ++ "." ++ sym ++ "_" ++ show n)


redefine :: BoM CompileState m => Symbol -> SymKey -> Object -> m ()
redefine symbol key obj = trace "redefine" $ do
    modify $ \s -> s { symTab = SymTab.insert symbol key obj (symTab s) }


define :: BoM CompileState m => Symbol -> SymKey -> Object -> m ()
define symbol key obj = trace "define" $ do
    checkSymKeyUndef symbol key
    redefine symbol key obj


checkSymKeyUndef :: BoM CompileState m => Symbol -> SymKey -> m ()
checkSymKeyUndef symbol key = trace ("checkSymKeyUndef " ++ show symbol) $ do
    res <- SymTab.lookupHead symbol key <$> gets symTab
    assert (isNothing res) (show symbol ++ " " ++ show key ++ " already defined")


checkSymUndef :: BoM CompileState m => Symbol -> m ()
checkSymUndef symbol = trace ("checkSymUndef " ++ show symbol) $ do
    res <- SymTab.lookupSym symbol <$> gets symTab
    assert (null res) (show symbol ++ " already defined")


addDeclared :: BoM CompileState m => LL.Name -> m ()
addDeclared name = trace "addDeclared" $ do
    modify $ \s -> s { declared = Set.insert name (declared s) }


addDeclaration :: BoM CompileState m => LL.Name -> Declaration -> m ()
addDeclaration name dec = trace "addDeclaration" $ do
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


emitDec :: ModCmp CompileState m => LL.Name -> Declaration -> m ()
emitDec name dec = trace "emitDec" $ case dec of
    DecExtern argTypes retty False  -> void $ extern name argTypes retty
    DecExtern argTypes retty True   -> void $ externVarArgs name argTypes retty
    DecVar opTyp                    ->
        emitDefn $ LL.GlobalDefinition $ globalVariableDefaults { name = name , type' = opTyp }


ensureDec :: ModCmp CompileState m => LL.Name -> m ()
ensureDec name = trace ("ensureDec " ++ show name) $ do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        resm <- Map.lookup name <$> gets declarations
        case catMaybes [resm] of
            [] -> return ()
            [d]  -> emitDec name d >> addDeclared name
            _    -> fail $ "ensureDec: " ++ (show name)


ensureSymKeyDec :: ModCmp CompileState m => Symbol -> SymKey -> m ()
ensureSymKeyDec symbol key = trace ("ensureSymKeyDec " ++ show symbol) $ do
    curMod <- gets curModName
    nm <- gets $ Map.lookup (symbol, key) . decMap
    case nm of
        Just name -> ensureDec name
        Nothing   -> do
            states <- gets imports
            rs <- fmap catMaybes $ forM states $ \state -> do
                case Map.lookup (symbol, key) (decMap state) of
                    Nothing   -> return Nothing
                    Just name -> return $ Just (name, (declarations state) Map.! name)

            case rs of
                []            -> return ()
                [(name, dec)] -> do
                    declared <- gets $ Set.member name . declared
                    when (not declared) $ do
                        emitDec name dec
                        addDeclared name
                _             -> fail ("More than one declaration for: " ++ show symbol)



ensureExtern :: ModCmp CompileState m => LL.Name -> [LL.Type] -> LL.Type -> Bool -> m LL.Operand
ensureExtern name argTypes retty isVarg = trace "ensureExtern" $ do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        addDeclaration name (DecExtern argTypes retty isVarg)
        ensureDec name
    
    return $ LL.ConstantOperand $
        C.GlobalReference (LL.ptr $ LL.FunctionType retty argTypes isVarg) name


lookm :: ModCmp CompileState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = do
    ensureSymKeyDec symbol key
    im <- gets $ map symTab . imports
    st <- gets symTab
    return $ lookupSymKey symbol key st im


look :: ModCmp CompileState m => Symbol -> SymKey -> m Object
look symbol key = do
    resm <- lookm symbol key
    assert (isJust resm) $ 
        "no definition for: " ++ show symbol ++ " " ++ show key
    return (fromJust resm)


lookSym :: ModCmp CompileState m => Symbol -> m [(SymKey, Object)]
lookSym symbol = do
    imports <- gets $ map symTab . imports
    symTab <- gets symTab
    return $ lookupSym symbol symTab imports

