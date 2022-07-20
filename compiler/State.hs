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
import LLVM.Context
import Foreign.Ptr

import qualified AST as S
import Type
import qualified SymTab
import qualified JIT
import Monad
import Error
import Trace


data Value
    = Val { valType :: Type, valOp :: LL.Operand }
    | Ptr { valType :: Type, valLoc :: LL.Operand }
    | ConstInt Integer
    deriving (Show, Eq)


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [Type]
    | KeyMember Type
    deriving (Eq, Ord)

instance Show SymKey where
    show KeyType = "type"
    show KeyVar  = "var"
    show (KeyFunc ts) = "fn(" ++ intercalate ", " (map show ts) ++ ")"
    show (KeyMember t) = show t ++ "."


data Object
    = ObjVal          Value
    | ObType          Type   (Maybe LL.Name)
    | ObjFunc         Type   LL.Operand
    | ObjConstructor  Type
    | ObjADTFieldCons Type
    | ObjMember       Int
    deriving ()


instance Show Object where
    show object = case object of
        ObjVal (Val t o)    -> "Val " ++ show t
        ObjVal (Ptr t o)    -> "Ptr " ++ show t
        ObjVal val          -> "val"
        ObType typ mn       -> show typ
        ObjFunc typ op      -> "fn(..)" ++ show typ
        ObjConstructor typ  -> "(..)" ++ show typ
        ObjADTFieldCons typ -> "Field:" ++ show typ
        ObjMember i         -> "." ++ show i


data Declaration
    = DecType   LL.Type
    | DecExtern [LL.Type] LL.Type Bool
    | DecFunc   [LL.Type] LL.Type
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
        }


mkBSS = BSS.toShort . BS.pack


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
    assert (isNothing res) (show symbol ++ " already defined")


checkSymUndef :: BoM CompileState m => Symbol -> m ()
checkSymUndef symbol = trace ("checkSymUndef " ++ show symbol) $ do
    res <- SymTab.lookupSym symbol <$> gets symTab
    assert (null res) (show symbol ++ " already defined")


addDeclared :: BoM CompileState m => LL.Name -> m ()
addDeclared name = trace "addDeclared" $ do
    modify $ \s -> s { declared = Set.insert name (declared s) }


addSymKeyDec :: BoM CompileState m => Symbol -> SymKey -> LL.Name -> Declaration -> m ()
addSymKeyDec symbol key name dec = trace ("addSymKeyDec " ++ show symbol) $ do
    modify $ \s -> s { decMap = Map.insert (symbol, key) name (decMap s) }
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


addDeclaration :: BoM CompileState m => LL.Name -> Declaration -> m ()
addDeclaration name dec = trace "addDeclaration" $ do
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


emitDec :: ModCmp CompileState m => LL.Name -> Declaration -> m ()
emitDec name dec = trace "emitDec" $ case dec of
    DecType opTyp                   -> void $ typedef name (Just opTyp)
    DecFunc argTypes retty          -> void $ extern name argTypes retty
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
    curMod <- gets curModName
    objm <- gets $ SymTab.lookup symbol key . symTab
    case objm of
        Just obj -> return (Just obj)
        Nothing -> do
            objs <- gets $ catMaybes . map (SymTab.lookup symbol key) . map symTab . imports
            case objs of
                []    -> return Nothing
                [obj] -> return (Just obj)
                _     -> fail ("Ambiguous symbol: " ++ show symbol)


look :: ModCmp CompileState m => Symbol -> SymKey -> m Object
look symbol key = do
    resm <- lookm symbol key
    assert (isJust resm) ("no definition for: " ++ show symbol ++ " " ++ show key)
    return (fromJust resm)
