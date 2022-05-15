{-# LANGUAGE FlexibleContexts #-}
module State where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Set as Set
import qualified Data.Map as Map
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
    deriving (Show, Eq)


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [Type]
    | KeyMember Type
    deriving (Eq, Ord)

instance Show SymKey where
    show KeyType = "(type)"
    show KeyVar  = "(variable)"
    show (KeyFunc ts) = "function(" ++ show ts ++ ")"
    show (KeyMember t) = show t ++ "."


data Object
    = ObjVal          Value
    | ObType          Type   (Maybe LL.Name)
    | ObjFunc         Type   LL.Operand
    | ObjConstructor  Type
    | ObjADTFieldCons Type
    | ObjMember       Int
    deriving (Show)


data Declaration
    = DecType   LL.Type
    | DecExtern [LL.Type] LL.Type Bool
    | DecFunc   [LL.Type] LL.Type
    | DecVar    LL.Type
    deriving (Show)


data CompileState
    = CompileState
        { imports       :: Map.Map S.ModuleName CompileState
        , decMap        :: Map.Map (String, SymKey) LL.Name
        , declarations  :: Map.Map LL.Name Declaration
        , declared      :: Set.Set LL.Name
        , symTab        :: SymTab.SymTab String SymKey Object
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
    return $ LL.Name $ mkBSS (mod ++ "." ++ sym ++ "_" ++ show n )


redefine :: BoM CompileState m => String -> SymKey -> Object -> m ()
redefine sym key obj = trace "redefine" $ do
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }

define :: BoM CompileState m => String -> SymKey -> Object -> m ()
define sym key obj = trace "define" $ do
    checkSymKeyUndef sym key
    redefine sym key obj


checkSymKeyUndef :: BoM CompileState m => String -> SymKey -> m ()
checkSymKeyUndef sym key = trace ("checkSymKeyUndef " ++ sym) $ do
    res <- SymTab.lookupHead sym key <$> gets symTab
    assert (isNothing res) (sym ++ " already defined")


checkSymUndef :: BoM CompileState m => String -> m ()
checkSymUndef sym = trace ("checkSymUndef " ++ sym) $ do
    res <- SymTab.lookupSym sym <$> gets symTab
    assert (null res) (sym ++ " already defined")


addDeclared :: BoM CompileState m => LL.Name -> m ()
addDeclared name = trace "addDeclared" $ do
    modify $ \s -> s { declared = Set.insert name (declared s) }


addSymKeyDec :: BoM CompileState m => String -> SymKey -> LL.Name -> Declaration -> m ()
addSymKeyDec sym key name dec = trace ("addSymKeyDec " ++ sym) $ do
    modify $ \s -> s { decMap = Map.insert (sym, key) name (decMap s) }
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
        case resm of
            Nothing -> return ()
            Just d  -> emitDec name d >> addDeclared name


ensureSymKeyDec :: ModCmp CompileState m => Symbol -> SymKey -> m ()
ensureSymKeyDec symbol key = trace ("ensureSymKeyDec " ++ show symbol) $ do
    curMod <- gets curModName
    case symbol of
        Sym sym -> do
            nm <- Map.lookup (sym, key) <$> gets decMap
            case nm of
                Just name -> ensureDec name
                Nothing   -> do
                    states <- Map.elems <$> gets imports
                    rs <- fmap concat $ forM states $ \state -> do
                        case Map.lookup (sym, key) (decMap state) of
                            Nothing   -> return []
                            Just name -> return [(name, (Map.! name) $ declarations state)]

                    case rs of
                        [] -> return ()
                        [(name, dec)] -> do
                            declared <- Set.member name <$> gets declared
                            when (not declared) $ do
                                emitDec name dec
                                addDeclared name
                        _ -> fail ("More than one declaration for: " ++ show symbol)


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
    imports <- gets imports
    curMod <- gets curModName
    case symbol of
        Sym sym -> do
            objm <- SymTab.lookup sym key <$> gets symTab
            let objsm = map (SymTab.lookup sym key) $ map symTab (Map.elems imports)

            case catMaybes (objm:objsm) of
                []  -> return Nothing
                [x] -> return (Just x)
                _   -> fail ("Ambiguous symbol: " ++ show sym)


look :: ModCmp CompileState m => Symbol -> SymKey -> m Object
look sym key = do
    resm <- lookm sym key
    assert (isJust resm) ("no definition for: " ++ show sym ++ " " ++ show key)
    return (fromJust resm)


pushSymTab :: BoM CompileState m => m ()
pushSymTab = trace "pushSymTab" $ do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM CompileState m => m ()
popSymTab = trace "popSymTab" $ do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }
