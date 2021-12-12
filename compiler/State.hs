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
import LLVM.AST hiding (function, type')
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C hiding (type')
import LLVM.AST.Type hiding (void)
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context
import Foreign.Ptr

import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified JIT
import Monad
import Error
import Trace


data Value
    = Val { valType :: T.Type, valOp :: Operand }
    | Ptr { valType :: T.Type, valLoc :: Operand }
    | Exp S.Expr -- Contextual
    deriving (Show, Eq)


valIsContextual :: Value -> Bool
valIsContextual (Exp _) = True
valIsContextual _       = False

exprIsContextual :: S.Expr -> Bool
exprIsContextual expr = case expr of
    S.Int _ _                              -> True
    S.Float _ _                            -> True
    S.Tuple _ es | any exprIsContextual es -> True
    S.Null _                               -> True
    S.Table _ ess | any null ess           -> True
    _                                      -> False

data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [T.Type]
    deriving (Show, Eq, Ord)


data Object
    = ObjVal          Value
    | ObType          T.Type   (Maybe Name)
    | ObjFunc         T.Type   Operand
    | ObjExtern       [T.Type] T.Type Operand
    | ObjConstructor  T.Type
    | ObjADTFieldCons T.Type
    deriving (Show)


data Declaration
    = DecType   Type
    | DecExtern [Type] Type Bool
    | DecFunc   [Type] Type
    | DecVar    Type
    deriving (Show)


data CompileState
    = CompileState
        { imports      :: Map.Map S.ModuleName CompileState
        , decMap       :: Map.Map (String, SymKey) Name
        , declarations :: Map.Map Name Declaration
        , declared     :: Set.Set Name
        , symTab       :: SymTab.SymTab String SymKey Object
        , curRetType   :: T.Type
        , curModName   :: String
        , nameMap      :: Map.Map String Int
        , posStack     :: [TextPos]
        }

initCompileState imports modName
     = CompileState
        { imports      = imports
        , decMap       = Map.empty
        , declarations = Map.empty
        , declared     = Set.empty
        , symTab       = SymTab.initSymTab
        , curRetType   = T.Void
        , curModName   = modName
        , nameMap      = Map.empty
        , posStack     = []
        }


mkBSS = BSS.toShort . BS.pack


myFresh :: InsCmp CompileState m => String -> m Name
myFresh sym = do
    nameMap <- gets nameMap
    mod <- gets curModName
    let n = maybe 0 (+1) (Map.lookup sym nameMap)
    modify $ \s -> s { nameMap = Map.insert sym n nameMap }
    return $ Name $ mkBSS (mod ++ "." ++ sym ++ "_" ++ show n )


assert :: BoM CompileState m => Bool -> String -> m ()
assert b s = do
    pos <- head <$> gets posStack
    unless b $ throwError (ErrorFile "" pos s)


err :: BoM CompileState m => String -> m a
err s = do
    pos <- head <$> gets posStack
    throwError (ErrorFile "" pos s)


withPos :: BoM CompileState m => TextPos -> m a -> m a
withPos pos f = do
    modify $ \s -> s { posStack = pos:(posStack s) }
    r <- f
    modify $ \s -> s { posStack = tail (posStack s) }
    return r


addObj :: BoM CompileState m => String -> SymKey -> Object -> m ()
addObj sym key obj = trace "addObj" $ do
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }

addObjWithCheck :: BoM CompileState m => String -> SymKey -> Object -> m ()
addObjWithCheck sym key obj = trace "addObjWithCheck" $ do
    checkSymKeyUndef sym key
    addObj sym key obj


checkSymKeyUndef :: BoM CompileState m => String -> SymKey -> m ()
checkSymKeyUndef sym key = trace ("checkSymKeyUndef " ++ sym) $ do
    res <- SymTab.lookupHead sym key <$> gets symTab
    when (isJust res) $ err (sym ++ " already defined")


checkSymUndef :: BoM CompileState m => String -> m ()
checkSymUndef sym = trace ("checkSymUndef " ++ sym) $ do
    res <- SymTab.lookupSym sym <$> gets symTab
    when (isJust res) $ err (sym ++ " already defined")


addDeclared :: BoM CompileState m => Name -> m ()
addDeclared name = trace "addDeclared" $ do
    modify $ \s -> s { declared = Set.insert name (declared s) }


addSymKeyDec :: BoM CompileState m => String -> SymKey -> Name -> Declaration -> m ()
addSymKeyDec sym key name dec = trace ("addSymKeyDec " ++ sym) $ do
    modify $ \s -> s { decMap = Map.insert (sym, key) name (decMap s) }
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


addDeclaration :: BoM CompileState m => Name -> Declaration -> m ()
addDeclaration name dec = trace "addDeclaration" $ do
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


emitDec :: ModCmp CompileState m => Name -> Declaration -> m ()
emitDec name dec = trace "emitDec" $ case dec of
    DecType opTyp                   -> void $ typedef name (Just opTyp)
    DecFunc argTypes retty          -> void $ extern name argTypes retty
    DecExtern argTypes retty False  -> void $ extern name argTypes retty
    DecExtern argTypes retty True   -> void $ externVarArgs name argTypes retty
    DecVar opTyp                    ->
        emitDefn $ GlobalDefinition $ globalVariableDefaults { name = name , type' = opTyp }


ensureDec :: ModCmp CompileState m => Name -> m ()
ensureDec name = trace ("ensureDec " ++ show name) $ do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        resm <- Map.lookup name <$> gets declarations
        case resm of
            Nothing -> return ()
            Just d  -> emitDec name d >> addDeclared name


ensureSymKeyDec :: ModCmp CompileState m => T.Symbol -> SymKey -> m ()
ensureSymKeyDec symbol key = trace ("ensureSymKeyDec " ++ show symbol) $ do
    curMod <- gets curModName
    case symbol of
        T.SymQualified mod sym | mod == curMod -> do
            nm <- Map.lookup (sym, key) <$> gets decMap
            maybe (return ()) ensureDec nm

        T.SymQualified mod sym -> do
            statem <- Map.lookup mod <$> gets imports
            when (isNothing statem) $ err ("No module: " ++ mod ++ " exists")

            let state = fromJust statem
            case Map.lookup (sym, key) (decMap state) of
                Nothing   -> return ()
                Just name -> do
                    declared <- Set.member name <$> gets declared
                    when (not declared) $ do
                        emitDec name $ (Map.! name) (declarations state)
                        addDeclared name

        T.Sym sym -> do
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
                        _ -> err ("More than one declaration for: " ++ show symbol)


ensureExtern :: ModCmp CompileState m => Name -> [Type] -> Type -> Bool -> m Operand
ensureExtern name argTypes retty isVarg = trace "ensureExtern" $ do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        addDeclaration name (DecExtern argTypes retty isVarg)
        ensureDec name
    
    return $ ConstantOperand $
        C.GlobalReference (ptr $ FunctionType retty argTypes isVarg) name


lookm :: ModCmp CompileState m => T.Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = do
    ensureSymKeyDec symbol key
    imports <- gets imports
    curMod <- gets curModName
    case symbol of
        T.SymQualified mod sym | mod == curMod ->
            SymTab.lookupSymKey sym key <$> gets symTab
            
        T.SymQualified mod sym ->
            case Map.lookup mod imports of
                Nothing    -> err ("No module: " ++ mod ++ " exists")
                Just state -> return $ SymTab.lookupSymKey sym key (symTab state)

        T.Sym sym -> do
            objm <- SymTab.lookupSymKey sym key <$> gets symTab
            let objsm = map (SymTab.lookupSymKey sym key) $ map symTab (Map.elems imports)

            case catMaybes (objm:objsm) of
                []  -> return Nothing
                [x] -> return (Just x)
                _   -> err ("Ambiguous symbol: " ++ show sym)


look :: ModCmp CompileState m => T.Symbol -> SymKey -> m Object
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
