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
        { context      :: Context
        , dataLayout   :: Ptr FFI.DataLayout
        , imports      :: Map.Map S.Path CompileState
        , decMap       :: Map.Map (S.Symbol, SymKey) Name
        , declarations :: Map.Map Name Declaration
        , declared     :: Set.Set Name
        , symTab       :: SymTab.SymTab S.Symbol SymKey Object
        , curRetType   :: T.Type
        , curModName   :: String
        , nameMap      :: Map.Map String Int
        , posStack     :: [TextPos]
        }

initCompileState ctx dl imports modName
     = CompileState
        { context      = ctx
        , dataLayout   = dl
        , imports      = imports
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
    n <- case Map.lookup sym nameMap of
        Nothing -> return 0
        Just n  -> return (n+1)

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


addObj :: BoM CompileState m => S.Symbol -> SymKey -> Object -> m ()
addObj sym key obj =
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }

addObjWithCheck :: BoM CompileState m => S.Symbol -> SymKey -> Object -> m ()
addObjWithCheck sym key obj = do
    checkSymKeyUndef sym key
    addObj sym key obj


checkSymKeyUndef :: BoM CompileState m => S.Symbol -> SymKey -> m ()
checkSymKeyUndef sym key = do
    res <- SymTab.lookupHead sym key <$> gets symTab
    when (isJust res) $ err (sym ++ " already defined")


checkSymUndef :: BoM CompileState m => S.Symbol -> m ()
checkSymUndef sym = do
    res <- SymTab.lookupSym sym <$> gets symTab
    when (isJust res) $ err (sym ++ " already defined")


addDeclared :: BoM CompileState m => Name -> m ()
addDeclared name =
    modify $ \s -> s { declared = Set.insert name (declared s) }


addSymKeyDec :: BoM CompileState m => S.Symbol -> SymKey -> Name -> Declaration -> m ()
addSymKeyDec sym key name dec = do
    modify $ \s -> s { decMap = Map.insert (sym, key) name (decMap s) }
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


addDeclaration :: BoM CompileState m => Name -> Declaration -> m ()
addDeclaration name dec = do
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


emitDec :: ModCmp CompileState m => Name -> Declaration -> m ()
emitDec name dec = case dec of
    DecType opTyp                   -> void $ typedef name (Just opTyp)
    DecFunc argTypes retty          -> emitDec name (DecExtern argTypes retty False)
    DecExtern argTypes retty False  -> void $ extern name argTypes retty
    DecExtern argTypes retty True   -> void $ externVarArgs name argTypes retty
    DecVar opTyp                    ->
        emitDefn $ GlobalDefinition $ globalVariableDefaults { name = name , type' = opTyp }


ensureDec :: ModCmp CompileState m => Name -> m ()
ensureDec name = do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        res <- Map.lookup name <$> gets declarations
        case res of
            Nothing -> return ()
            Just d  -> emitDec name d >> addDeclared name


ensureSymKeyDec :: ModCmp CompileState m => S.Symbol -> SymKey -> m ()
ensureSymKeyDec sym key = do
    nm <- Map.lookup (sym, key) <$> gets decMap
    case nm of
        Just name -> ensureDec name
        Nothing   -> do
            imports <- Map.elems <$> gets imports
            rs <- fmap catMaybes $ forM imports $ \imp -> do
                case Map.lookup (sym, key) (decMap imp) of
                    Nothing   -> return Nothing
                    Just name -> do
                        declared <- Set.member name <$> gets declared
                        when (not declared) $ do
                            emitDec name ((Map.! name) $ declarations imp)
                            addDeclared name
                        return (Just ())

            case rs of
                []    -> return ()
                [r]   -> return ()
                (r:_) -> fail ("more than one declaration for: " ++ sym ++ " " ++ show key)


ensureExtern :: ModCmp CompileState m => Name -> [Type] -> Type -> Bool -> m Operand
ensureExtern name argTypes retty isVarg = do
    declared <- Set.member name <$> gets declared
    when (not declared) $ do
        addDeclaration name (DecExtern argTypes retty isVarg)
        ensureDec name
    
    return $ ConstantOperand $
        C.GlobalReference (ptr $ FunctionType retty argTypes isVarg) name


lookm :: ModCmp CompileState m => S.Symbol -> SymKey -> m (Maybe Object)
lookm sym key = do
    ensureSymKeyDec sym key
    res <- SymTab.lookupSymKey sym key <$> gets symTab
    case res of
        Just obj -> return (Just obj)
        Nothing  -> do
            r <- (catMaybes . map (SymTab.lookupSymKey sym key . symTab) . Map.elems) <$> gets imports
            case r of
                []  -> return Nothing
                [x] -> return (Just x)

look :: ModCmp CompileState m => S.Symbol -> SymKey -> m Object
look sym key = do
    res <- lookm sym key
    case res of
        Nothing -> err ("no definition for: " ++ sym ++ " " ++ show key)
        Just x  -> return x


pushSymTab :: BoM CompileState m => m ()
pushSymTab =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM CompileState m => m ()
popSymTab =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }
