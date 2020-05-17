{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}


module CmpMonad where

import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe

import           LLVM.AST 
import           LLVM.AST.Global
import           LLVM.AST.Constant          as C
import           LLVM.AST.Type              hiding (void)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified SymTab


class (Ord k, MonadError CmpError m, MonadState (CmpState k o) m, MonadModuleBuilder m) => MonadModuleCmp k o m
class (MonadModuleCmp k o m, MonadIRBuilder m) => MonadInstrCmp k o m

instance (Ord k, Monad m) => (MonadModuleCmp k o) (ModuleCmpT k o m)
instance (Ord k, Monad m) => (MonadModuleCmp k o) (InstrCmpT k o m)
instance (Ord k, Monad m) => (MonadInstrCmp k o) (InstrCmpT k o m)

instance MonadTrans (ModuleCmpT k o) where
    lift = ModuleCmpT . ModuleBuilderT . lift . lift . ExceptT . (fmap Right)

instance MonadTrans (InstrCmpT k o) where
    lift = InstrCmpT . IRBuilderT . lift . lift

instance (Monad m, Ord k) => MonadFail (InstrCmpT k o m) where
    fail = cmpErr

type ModuleCmp k o = ModuleCmpT k o Identity
newtype ModuleCmpT k o m a
    = ModuleCmpT { getModuleCmp :: ModuleBuilderT (StateT (CmpState k o) (ExceptT CmpError m)) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError CmpError, MonadState (CmpState k o)
        , MonadModuleBuilder
        )


type InstrCmp k o = InstrCmpT k o Identity
newtype InstrCmpT k o m a
    = InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT k o m) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError CmpError, MonadState (CmpState k o)
        , MonadModuleBuilder, MonadIRBuilder
        )


data TextPos
    = TextPos { textPos, textLine, textCol :: Int }
    deriving (Eq)


instance Show TextPos where
    show (TextPos p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


data CmpState k o
    = CmpState
        { posStack :: [TextPos]
        , symTab   :: SymTab.SymTab String (Map.Map k (o, Set.Set Name))
        , actions  :: Map.Map Name (ModuleBuilder ())
        , declared :: Set.Set Name
        , exported :: Set.Set Name
        }
    deriving ()


initCmpState = CmpState
    { posStack = [TextPos 1 1 1]
    , symTab   = SymTab.initSymTab
    , actions  = Map.empty
    , declared = Set.empty
    , exported = Set.empty
    }


newtype CmpError
    = CmpError { getCmpError :: (TextPos, String) }
    deriving (Show)


runModuleCmpT
    :: Monad m
    => ModuleBuilderState
    -> CmpState k o
    -> ModuleCmpT k o m a
    -> m (Either CmpError ((a, [Definition]), CmpState k o))
runModuleCmpT moduleBuilderState cmpState moduleCmpT =
    runExceptT $ (flip runStateT) cmpState $
        runModuleBuilderT moduleBuilderState $ getModuleCmp moduleCmpT


runModuleCmp
    :: ModuleBuilderState
    -> CmpState k o
    -> ModuleCmp k o a
    -> Either CmpError ((a, [Definition]), CmpState k o)
runModuleCmp moduleBuilderState cmpState moduleCmpT =
    runIdentity $ runExceptT $ (flip runStateT) cmpState $
        runModuleBuilderT moduleBuilderState $ getModuleCmp moduleCmpT


cmpErr:: MonadModuleCmp k o m => String -> m a
cmpErr str = do
    pos <- fmap head (gets posStack)
    throwError $ CmpError (pos, str)


assert :: MonadModuleCmp k o m => Bool -> String -> m ()
assert cnd str =
    unless cnd $ void (cmpErr str)



withPos :: MonadModuleCmp k o m => TextPos -> m a -> m a
withPos pos f = do
    modify $ \s -> s { posStack = pos:(posStack s) }
    r <- f
    modify $ \s -> s { posStack = tail (posStack s) }
    return r


checkUndefined :: MonadModuleCmp k o m => String -> m ()
checkUndefined symbol = do
    st  <- gets symTab
    let res = SymTab.lookup symbol [head st]
    assert (isNothing res) (symbol ++ " already defined") 


checkSymKeyUndefined :: MonadModuleCmp k o m => String -> k -> m ()
checkSymKeyUndefined symbol key = do
    res <- lookupSymKey symbol key
    assert (isNothing res) (symbol ++ " already defined")


lookupSymbol :: MonadModuleCmp k o m => String -> m (Maybe (Map.Map k (o, Set.Set Name)))
lookupSymbol symbol =
    fmap (SymTab.lookup symbol) (gets symTab)


lookupSymKey :: MonadModuleCmp k o m => String -> k -> m (Maybe o)
lookupSymKey symbol key = do
    keyMap <- lookupSymbol symbol
    case keyMap of
        Nothing -> return Nothing
        Just km -> case Map.lookup key km of
            Nothing    -> return Nothing
            Just (o,_) -> return (Just o)


look :: (Show k, Show o, MonadModuleCmp k o m) => String -> k -> m o
look symbol key = do
    keyMap <- lookupSymbol symbol 
    assert (isJust keyMap) (symbol ++ " doesn't exist")
    let entry = Map.lookup key (fromJust keyMap)
    assert (isJust entry) ("no matching entry for symbol")
    let Just (obj, nameSet) = entry 
    mapM_ ensureDef (Set.toList nameSet)
    return obj


addSymObj :: MonadModuleCmp k o m => String -> k -> o -> m ()
addSymObj symbol key obj = do
    keyMap <- lookupSymbol symbol
    let keyMap' = Map.insert key (obj, Set.empty) (maybe Map.empty id keyMap)
    modify $ \s -> s { symTab = SymTab.insert symbol keyMap' (symTab s) }


addSymObjReq :: MonadModuleCmp k o m => String -> k -> Name -> m ()
addSymObjReq symbol key name = do
    keyMap <- fmap fromJust (lookupSymbol symbol)
    let (obj, nameSet) = fromJust (Map.lookup key keyMap)
    let keyMap' = Map.insert key (obj, Set.insert name nameSet) keyMap
    modify $ \s -> s { symTab = SymTab.insert symbol keyMap' (symTab s) }


pushSymTab :: MonadModuleCmp k o m => m ()
pushSymTab =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: MonadModuleCmp k o m => m ()
popSymTab =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


addAction :: MonadModuleCmp k o m => Name -> ModuleBuilder a -> m ()
addAction name action =
    modify $ \s -> s { actions = Map.insert name (void action) (actions s) }


lookupAction :: MonadModuleCmp k o m => Name -> m (Maybe (ModuleBuilder ()))
lookupAction name =
    fmap (Map.lookup name) (gets actions)


ensureDef :: MonadModuleCmp k o m => Name -> m ()
ensureDef name = do
    declared <- isDeclared name
    unless declared $ do
        action <- fmap fromJust (lookupAction name)
        liftModuleState (unModuleBuilderT action)
        addDeclared name


ensureSymDeps :: MonadModuleCmp k o m => String -> k -> m ()
ensureSymDeps symbol key = do
    keyMap <- lookupSymbol symbol
    assert (isJust keyMap) (symbol ++ " wasn't found")
    let Just (obj, nameSet) = Map.lookup key (fromJust keyMap)
    mapM_ ensureDef (Set.toList nameSet)
    

ensureExtern :: MonadModuleCmp k o m => Name -> [Type] -> Type -> Bool -> m Operand
ensureExtern name paramTypes returnType isVarg = do
    exists <- lookupAction name
    unless (isJust exists) (addExtern name paramTypes returnType isVarg)
    ensureDef name
    return $ ConstantOperand $
        GlobalReference (ptr $ FunctionType returnType paramTypes isVarg) name


addExtern :: MonadModuleCmp k o m => Name -> [Type] -> Type -> Bool -> m ()
addExtern name paramTypes retType isVarg = do
    addAction name $ emitDefn $ GlobalDefinition $ functionDefaults
        { returnType = retType
        , name       = name
        , parameters = ([Parameter typ (mkName "") [] | typ <- paramTypes], isVarg)
        }


addDeclared :: MonadModuleCmp k o m => Name -> m ()
addDeclared name =
    modify $ \s -> s { declared = Set.insert name (declared s) }


addExported :: MonadModuleCmp k o m => Name -> m ()
addExported name =
    modify $ \s -> s { exported = Set.insert name (exported s) }


isDeclared :: MonadModuleCmp k o m => Name -> m Bool
isDeclared name =
    fmap (elem name) (gets declared)


prettySymTab :: (Show k, Show o) => CmpState k o -> IO ()
prettySymTab state = do
    let st = symTab state
    forM_ (zip st [0..]) $ \(frame, i) -> do
        putStrLn ("frame " ++ show i ++ ":")
        forM_ (Map.toList frame) $ \(sym, keyMap) -> do
            putStrLn ("  " ++ sym ++ ":")
            forM_ (Map.toList keyMap) $ \(key, obj) -> do
                let kstr = show key
                putStrLn ("    " ++ show key ++ " " ++ (replicate (30-length kstr) '-') ++ "> " ++ take 90 (show obj) ++ "...")

