{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified SymTab
import qualified Type as T


type Symbol = String
type Name   = String

newtype ModuleT m a
    = ModuleT { getModule :: StateT ModuleState m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState ModuleState)


fresh :: MonadModule m => Symbol -> m Name
fresh symbol = do
    supply <- gets moduleSupply
    let i = maybe 0 (+1) (Map.lookup symbol supply)
    modify $ \s -> s { moduleSupply = Map.insert symbol i supply }
    return (symbol ++ "_" ++ show i)


addDef :: MonadModule m => Name -> Definition -> m ()
addDef name def =
    modify $ \s -> s { moduleDefs = Map.insert name def (moduleDefs s) }


addModuleSym :: MonadModule m => Symbol -> Name -> m ()
addModuleSym symbol name =
    modify $ \s -> s { moduleSymbols = Map.insert symbol name (moduleSymbols s) }


lookupModuleSym :: MonadModule m => Symbol -> m (Maybe Name)
lookupModuleSym symbol =
    fmap (Map.lookup symbol) (gets moduleSymbols)


lookupModuleDef :: MonadModule m => Name -> m (Maybe Definition)
lookupModuleDef name =
    fmap (Map.lookup name) (gets moduleDefs)


prettyModuleState :: ModuleState -> IO ()
prettyModuleState moduleState = do
    forM_ (Map.toList $ moduleDefs moduleState) $ \(name, def) -> do
        putStr (name ++ ": ")
        case def of
            Function instructions -> do
                putStrLn "function"
                forM_ instructions $ \i -> putStrLn ("\t" ++ show i)
            Variable _ -> putStrLn (show def)


newtype InstrT m a
    = InstrT { getInstr :: StateT InstrState m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState InstrState)


addInstr :: MonadInstr m => Instruction -> m ()
addInstr instr =
    modify $ \s -> s { instrs = (instrs s) ++ [instr] }


addSym :: MonadInstr m => Symbol -> Name -> m ()
addSym symbol name = do
    sc <- fmap head (gets scope)
    let sc' = sc { symbols = Map.insert symbol name (symbols sc) }
    modify $ \s -> s { scope = sc' : tail (scope s) }


lookupSym :: MonadInstr m => Symbol -> m (Maybe Name)
lookupSym symbol =
    fmap lookupSym' (gets scope)
    where
        lookupSym' :: [ScopeState] -> Maybe Name
        lookupSym' []      = Nothing
        lookupSym' (s:ss)  = case Map.lookup symbol (symbols s) of
            Nothing -> lookupSym' ss
            Just nm -> Just nm
        
    
pushScope, popScope :: MonadInstr m => m ()
pushScope = modify $ \s -> s { scope = initScopeState : scope s }
popScope  = modify $ \s -> s { scope = tail (scope s) }


class (MonadState ModuleState m) => MonadModule m
class (MonadState InstrState m) => MonadInstr m


instance (Monad m) => MonadModule (ModuleT m)
instance (Monad m) => MonadInstr (InstrT m)


data ModuleState
    = ModuleState
        { moduleDefs    :: Map.Map Name Definition
        , moduleExports :: Set.Set Name
        , moduleSupply  :: Map.Map Symbol Int
        , moduleSymbols :: Map.Map Symbol Name
        }
    deriving Show


initModuleState =
    ModuleState
        { moduleDefs    = Map.empty
        , moduleExports = Set.empty
        , moduleSupply  = Map.empty
        , moduleSymbols = Map.empty
        }


data InstrState
    = InstrState
        { scope    :: [ScopeState]
        , instrs   :: [Instruction]
        }
    deriving Show


initInstrState =
    InstrState
        { scope    = [initScopeState]
        , instrs   = []
        }


data Definition
    = Function [Instruction]
    | Variable T.Type 
    deriving Show


data Value
    = Cons S.Constant
    | Named Name
    | Infix S.Op Value Value
    deriving Show


data Instruction
    = Store Name Value
    | If Value [Instruction] [Instruction]
    | Assert String
    | Print [Value]
    deriving Show


data ScopeState
    = ScopeState
        { symbols  :: Map.Map Symbol Name
        , deferred :: [Instruction]
        }
    deriving Show


initScopeState =
    ScopeState
        { symbols  = Map.empty
        , deferred = []
        }


function :: MonadModule m => Name -> [()] -> InstrT m () -> m ()
function name params cmp = do
    ins <- fmap instrs $ execStateT (getInstr cmp) initInstrState
    addDef name (Function ins)


cmpAST :: S.AST -> IO ModuleState
cmpAST ast = execStateT (getModule cmp) initModuleState
    where
        cmp :: ModuleT IO ()
        cmp = do
            name <- fresh "main"
            function name [] (mapM_ cmpTopStmt ast)


cmpIf :: MonadModule m => Value -> InstrT m () -> InstrT m () -> InstrT m ()
cmpIf (Cons (S.Bool _ True)) true _   = true
cmpIf (Cons (S.Bool _ False)) _ false = false
cmpIf cnd true false                  = do
    trueInstr <- lift $ execStateT (getInstr true) initInstrState
    falseInstr <- lift $ execStateT (getInstr false) initInstrState 
    addInstr $ If cnd (instrs trueInstr) (instrs falseInstr)
    

cmpTopPattern :: MonadModule m => S.Pattern -> Value -> InstrT m Value
cmpTopPattern pattern val = case pattern of
    S.PatIgnore pos    -> return $ Cons (S.Bool pos True)
    S.PatIdent pos sym -> do
        name <- lift (fresh sym)
        lift (addModuleSym sym name)
        if isCons val then
            lift $ addDef name (Variable T.I64)
        else do
            lift $ addDef name (Variable T.I64)
            addInstr (Store name val)
        return $ Cons (S.Bool pos True)
    

cmpTopStmt :: MonadModule m => S.Stmt -> InstrT m ()
cmpTopStmt stmt = case stmt of
    S.Assign pos pattern expr -> do
        val <- cmpExpr expr
        matched <- cmpTopPattern pattern val
        cmpIf matched (return ()) (addInstr $ Assert "pattern failure")

    S.Print pos exprs ->
        addInstr . Print =<< mapM cmpExpr exprs
        


cmpExpr :: MonadModule m => S.Expr -> InstrT m Value
cmpExpr expr = case expr of
    S.Cons c -> return (Cons c)
    S.Ident pos sym -> do
        name <- lookupSym sym
        case name of
            Just nm -> return (Named nm)
            Nothing -> do
                modName <- lift (lookupModuleSym sym)
                maybe (error "") (return . Named) modName

    S.Tuple pos exprs -> do
        vals <- mapM cmpExpr exprs
        error "todo"
        return $ Cons (S.Int pos 4)

    S.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        return (Infix op valA valB)



isCons :: Value -> Bool
isCons (Cons _) = True
isCons _        = False


toCons :: Value -> S.Constant
toCons (Cons c) = c

