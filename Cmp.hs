{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}


module Cmp where

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
    fail = cmpErr (TextPos 0 0 0)

type ModuleCmp k o = ModuleCmpT k o Identity
newtype ModuleCmpT k o m a
    = ModuleCmpT { getModuleCmp :: ModuleBuilderT (StateT (CmpState k o) (ExceptT CmpError m)) a }
    deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState (CmpState k o), MonadModuleBuilder)


type InstrCmp k o = InstrCmpT k o Identity
newtype InstrCmpT k o m a
    = InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT k o m) a }
    deriving (Functor, Applicative, Monad, MonadError CmpError, MonadState (CmpState k o), MonadModuleBuilder, MonadIRBuilder) 


data CmpState k o
    = CmpState
        { curFn    :: Name
        , symTab   :: SymTab.SymTab String (Map.Map k (o, Set.Set Name))
        , defs     :: Map.Map Name Definition
        , declared :: Set.Set Name
        , exported :: Set.Set Name
        }
    deriving (Show)


data TextPos = TextPos { textPos, textLine, textCol :: Int } deriving (Show, Eq)


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


initCmpState = CmpState
    { curFn    = mkName ""
    , symTab   = SymTab.initSymTab
    , defs     = Map.empty
    , declared = Set.empty
    , exported = Set.empty
    }


cmpErr:: MonadModuleCmp k o m => TextPos -> String -> m a
cmpErr pos str =
    throwError $ CmpError (pos, str)


checkUndefined :: MonadModuleCmp k o m => TextPos -> String -> m ()
checkUndefined pos symbol = do
    res <- lookupSymbol symbol
    unless (isNothing res) $ cmpErr pos (symbol ++ " already defined") 


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


look :: (Show k, Show o, MonadModuleCmp k o m) => TextPos -> String -> k -> m o
look pos symbol key = do
    keyMap <- lookupSymbol symbol 
    unless (isJust keyMap) $ cmpErr pos (symbol ++ " doesn't exist")
    let entry = Map.lookup key (fromJust keyMap)
    unless (isJust entry) $ cmpErr pos ("no matching entry for symbol")
    let (obj, nameSet) = fromJust entry 
    unless (Set.null nameSet) $ mapM_ ensureDef (Set.toList nameSet)
    return obj


addSymObj :: MonadModuleCmp k o m => String -> k -> o -> m ()
addSymObj symbol key obj = do
    keyMap <- lookupSymbol symbol
    let keyMap' = Map.insert key (obj, Set.empty) (maybe Map.empty id keyMap)
    modify $ \s -> s { symTab = SymTab.insert symbol keyMap' (symTab s) }


addSymObjRequirement :: MonadModuleCmp k o m => String -> k -> Name -> m ()
addSymObjRequirement symbol key name = do
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


addDef :: MonadModuleCmp k o m => Name -> Definition -> m ()
addDef name def =
    modify $ \s -> s { defs = Map.insert name def (defs s) }


lookupDef :: MonadModuleCmp k o m => Name -> m (Maybe Definition)
lookupDef name =
    fmap (Map.lookup name) (gets defs)


ensureDef :: MonadModuleCmp k o m => Name -> m ()
ensureDef name = do
    declared <- isDeclared name
    unless declared $ do
        emitDefn =<< fmap fromJust (lookupDef name)
        addDeclared name


addExtern :: MonadModuleCmp k o m => Name -> [Type] -> Type -> Bool -> m ()
addExtern name paramTypes retType isVarg = do
    addDef name $ GlobalDefinition $ functionDefaults
        { returnType = retType
        , name       = name
        , parameters = ([Parameter typ (mkName "") [] | typ <- paramTypes], isVarg)
        }


ensureExtern :: MonadModuleCmp k o m => Name -> [Type] -> Type -> Bool -> m Operand
ensureExtern name paramTypes returnType isVarg = do
    exists <- lookupDef name
    unless (isJust exists) (addExtern name paramTypes returnType isVarg)
    ensureDef name
    return $ ConstantOperand $
        GlobalReference (ptr $ FunctionType returnType paramTypes isVarg) name


addDeclared :: MonadModuleCmp k o m => Name -> m ()
addDeclared name =
    modify $ \s -> s { declared = Set.insert name (declared s) }


getCurFnName :: MonadModuleCmp k o m => m Name
getCurFnName =
    gets curFn


setCurFnName :: MonadModuleCmp k o m => Name -> m ()
setCurFnName name =
    modify $ \s -> s { curFn = name }


addExported :: MonadModuleCmp k o m => Name -> m ()
addExported name =
    modify $ \s -> s { exported = Set.insert name (exported s) }


isDeclared :: MonadModuleCmp k o m => Name -> m Bool
isDeclared name =
    fmap (elem name) (gets declared)



