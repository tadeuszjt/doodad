{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Control.Monad.State
import Control.Monad.Except hiding (void)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified SymTab
import qualified Type as T


type Symbol = String
type Name   = String

data ResolverState
    = ResolverState
        {
        }
    deriving Show


data ResolverError
    = ResolverError
        {
        }
    deriving Show


newtype ResolverT m a
    = ResolverT { getResolver :: StateT ResolverState (ExceptT ResolverError m) a }
    deriving (Functor, Applicative, Monad)

runResolverT :: Monad m => ResolverState -> ResolverT m a -> m (Either ResolverError (a, ResolverState))
runResolverT resolverState resolverT =
    runExceptT $ (flip runStateT) resolverState $ (getResolver resolverT) 

--
--newtype ModuleT m a
--    = ModuleT { getModule :: StateT ModuleState m a }
--    deriving (Functor, Applicative, Monad, MonadTrans, MonadState ModuleState)
--
--
--fresh :: MonadModule m => Symbol -> m Name
--fresh symbol = do
--    supply <- gets moduleSupply
--    let i = maybe 0 (+1) (Map.lookup symbol supply)
--    modify $ \s -> s { moduleSupply = Map.insert symbol i supply }
--    return (symbol ++ "_" ++ show i)
--
--
--addDef :: MonadModule m => Name -> Definition -> m ()
--addDef name def =
--    modify $ \s -> s { moduleDefs = Map.insert name def (moduleDefs s) }
--
--
--addModuleSym :: MonadModule m => Symbol -> Name -> m ()
--addModuleSym symbol name =
--    modify $ \s -> s { moduleSymbols = Map.insert symbol name (moduleSymbols s) }
--
--
--lookupModuleSym :: MonadModule m => Symbol -> m (Maybe Name)
--lookupModuleSym symbol =
--    fmap (Map.lookup symbol) (gets moduleSymbols)
--
--
--lookupModuleDef :: MonadModule m => Name -> m (Maybe Definition)
--lookupModuleDef name =
--    fmap (Map.lookup name) (gets moduleDefs)
--
--
--checkModSymUndefined :: MonadModule m => Symbol -> m ()
--checkModSymUndefined symbol = do
--    symbols <- gets moduleSymbols
--    maybe (return ()) (error "defined") (Map.lookup symbol symbols)
--
--
--prettyModuleState :: ModuleState -> IO ()
--prettyModuleState moduleState = do
--    forM_ (Map.toList $ moduleDefs moduleState) $ \(name, def) -> do
--        putStr (name ++ ": ")
--        case def of
--            Function instructions -> do
--                putStrLn "function"
--                mapM_ (prettyInstr "\t") instructions
--            Variable _ -> putStrLn (show def)
--    where
--        prettyInstr :: String -> Instruction -> IO ()
--        prettyInstr app instr = case instr of
--            While cnd instrs -> do
--                putStrLn (app ++ "while " ++ show cnd)
--                forM_ instrs $ \ins -> prettyInstr ("\t" ++ app) ins
--            ins -> putStr app >> putStrLn (show ins)
--
--
--newtype InstrT m a
--    = InstrT { getInstr :: StateT InstrState m a }
--    deriving (Functor, Applicative, Monad, MonadTrans, MonadState InstrState)
--
--
--addInstr :: MonadInstr m => Instruction -> m ()
--addInstr instr = do
--    sc <- fmap head (gets scope)
--    modify $ \s -> s { scope = (sc { instrs = (instrs sc) ++ [instr] }) : (tail $ scope s) }
--
--
--addSym :: MonadInstr m => Symbol -> Name -> m ()
--addSym symbol name = do
--    sc <- fmap head (gets scope)
--    let sc' = sc { symbols = Map.insert symbol name (symbols sc) }
--    modify $ \s -> s { scope = sc' : tail (scope s) }
--
--
--lookupSym :: MonadInstr m => Symbol -> m (Maybe Name)
--lookupSym symbol =
--    fmap lookupSym' (gets scope)
--    where
--        lookupSym' :: [ScopeState] -> Maybe Name
--        lookupSym' []      = Nothing
--        lookupSym' (s:ss)  = case Map.lookup symbol (symbols s) of
--            Nothing -> lookupSym' ss
--            Just nm -> Just nm
--
--
--checkSymUndefined :: MonadInstr m => Symbol -> m ()
--checkSymUndefined symbol =
--    maybe (return ()) (error "defined") =<< lookupSym symbol
--
--
--pushScope :: MonadInstr m => m ()
--pushScope =
--    modify $ \s -> s { scope = initScopeState : scope s }
--
--
--popScope :: MonadInstr m => m [Instruction]
--popScope  = do
--    ins <- fmap (instrs . head) (gets scope)
--    modify $ \s -> s { scope = tail (scope s) }
--    return ins
--
--
--look :: MonadModule m => Symbol -> InstrT m Name
--look symbol = do
--    name <- lookupSym symbol
--    case name of
--        Just nm -> return nm
--        Nothing -> do
--            modName <- lift (lookupModuleSym symbol)
--            maybe (error "undefined") return modName
--
--
--class (MonadState ModuleState m) => MonadModule m
--class (MonadState InstrState m) => MonadInstr m
--
--
--instance (Monad m) => MonadModule (ModuleT m)
--instance (Monad m) => MonadInstr (InstrT m)
--instance (Monad m) => MonadFail (InstrT m) where
--    fail s = error s
--
--
--data ModuleState
--    = ModuleState
--        { moduleDefs    :: Map.Map Name Definition
--        , moduleExports :: Set.Set Name
--        , moduleSupply  :: Map.Map Symbol Int
--        , moduleSymbols :: Map.Map Symbol Name
--        }
--    deriving Show
--
--
--initModuleState =
--    ModuleState
--        { moduleDefs    = Map.empty
--        , moduleExports = Set.empty
--        , moduleSupply  = Map.empty
--        , moduleSymbols = Map.empty
--        }
--
--
--data InstrState
--    = InstrState
--        { scope :: [ScopeState]
--        , types :: Map.Map Name T.Type
--        }
--    deriving Show
--
--
--initInstrState =
--    InstrState
--        { scope = [initScopeState]
--        , types = Map.empty
--        }
--
--
--data ScopeState
--    = ScopeState
--        { symbols  :: Map.Map Symbol Name
--        , instrs   :: [Instruction]
--        , deferred :: [Instruction]
--        }
--    deriving Show
--
--
--initScopeState =
--    ScopeState
--        { symbols  = Map.empty
--        , instrs   = []
--        , deferred = []
--        }
--
--
--data Definition
--    = Function [Instruction]
--    | Variable (Maybe S.Constant)
--    deriving Show
--
--
--data Value
--    = Cons S.Constant
--    | Tuple [Value]
--    | Array [S.Constant]
--    | Named Name
--    | Infix S.Op Value Value
--    deriving Show
--
--
--data Instruction
--    = Store Name Value
--    | Alloca Name
--    | If Value [Instruction] [Instruction]
--    | Assert String
--    | Print [Value]
--    | While Value [Instruction]
--    deriving Show
--
--
--function :: MonadModule m => Name -> [()] -> InstrT m () -> m ()
--function name params cmp = do
--    ins <- fmap (instrs . head . scope) $ execStateT (getInstr cmp) initInstrState
--    addDef name (Function ins)
--
--
--cmpAST :: S.AST -> IO ModuleState
--cmpAST ast = execStateT (getModule cmp) initModuleState
--    where
--        cmp :: ModuleT IO ()
--        cmp = do
--            name <- fresh "main"
--            function name [] (mapM_ cmpTopStmt ast)
--
--
--cmpIf :: MonadModule m => Value -> InstrT m () -> InstrT m () -> InstrT m ()
--cmpIf (Cons (S.Bool _ True)) true _   = pushScope >> true >> popScope >>= mapM_ addInstr 
--cmpIf (Cons (S.Bool _ False)) _ false = pushScope >> false >> popScope >>= mapM_ addInstr
--cmpIf cnd true false                  = do
--    trueIns <- pushScope >> true >> popScope
--    falseIns <- pushScope >> false >> popScope
--    addInstr (If cnd trueIns falseIns)
--    
--
--cmpTopPattern :: MonadModule m => S.Pattern -> Value -> InstrT m Value
--cmpTopPattern pattern val = case pattern of
--    S.PatIgnore pos    -> return $ Cons (S.Bool pos True)
--    S.PatIdent pos sym -> do
--        lift (checkModSymUndefined sym)
--        name <- lift (fresh sym)
--        lift (addModuleSym sym name)
--        if isCons val then
--            lift $ addDef name $ Variable (Just $ toCons val)
--        else do
--            lift $ addDef name (Variable Nothing)
--            addInstr (Store name val)
--        return $ Cons (S.Bool pos True)
--    
--
--cmpTopStmt :: MonadModule m => S.Stmt -> InstrT m ()
--cmpTopStmt stmt = case stmt of
--    S.Print pos exprs -> cmpStmt stmt
--
--    S.Assign pos pattern expr -> do
--        val <- cmpExpr expr
--        matched <- cmpTopPattern pattern val
--        cmpIf matched (return ()) (addInstr $ Assert "pattern failure")
--
--    S.While pos cnd block -> do
--        cndVal <- cmpExpr cnd
--        ins <- pushScope >> mapM_ cmpStmt block >> popScope
--        addInstr (While cndVal ins)
--        
--
--cmpPattern :: MonadModule m => S.Pattern -> Value -> InstrT m Value
--cmpPattern pattern val = case pattern of
--    S.PatIgnore pos    -> return $ Cons (S.Bool pos True)
--    S.PatIdent pos sym -> do
--        checkSymUndefined sym
--        name <- lift (fresh sym)
--        addInstr (Alloca name)
--        addInstr (Store name val)
--        return $ Cons (S.Bool pos True)
--
--
--cmpIndex :: MonadModule m => S.Index -> Value -> InstrT m ()
--cmpIndex index val = case index of
--    S.IndIdent pos symbol -> do
--        name <- look symbol
--        addInstr (Store name val)
--        
--    
--cmpStmt :: MonadModule m => S.Stmt -> InstrT m ()
--cmpStmt stmt = case stmt of
--    S.Print pos exprs    -> addInstr . Print =<< mapM cmpExpr exprs
--    S.Set pos index expr -> cmpIndex index =<< cmpExpr expr
--
--    S.Assign pos pattern expr -> do
--        matched <- cmpPattern pattern =<< cmpExpr expr
--        cmpIf matched (return ()) (addInstr $ Assert "pattern failure")
--
--
--cmpExpr :: MonadModule m => S.Expr -> InstrT m Value
--cmpExpr expr = case expr of
--    S.Cons c          -> return (Cons c)
--    S.Ident pos sym   -> fmap Named (look sym)
--    S.Tuple pos exprs -> fmap Tuple (mapM cmpExpr exprs)
--
--    S.Infix pos op exprA exprB -> do
--        valA <- cmpExpr exprA
--        valB <- cmpExpr exprB
--        return (Infix op valA valB)
--
--
--
--isCons :: Value -> Bool
--isCons (Cons _)     = True
--isCons (Tuple vals) = all isCons vals
--isCons _            = False
--
--
--toCons :: Value -> S.Constant
--toCons (Cons c) = c
--
