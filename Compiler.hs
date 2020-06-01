{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import qualified AST as S
import qualified SymTab
import qualified Type as T


type Symbol = String
type Name   = String


data ModuleState
    = ModuleState
        { moduleDefs  :: Map.Map Name Definition
        , symbolUsage :: Map.Map Symbol Int
        }
    deriving Show


data InstrState
    = InstrState
        { deferred :: [Instruction]
        , instrs   :: [Instruction]
        }
    deriving Show


initModuleState =
    ModuleState
        { moduleDefs  = Map.empty
        , symbolUsage = Map.empty
        }


initInstrState =
    InstrState
        { instrs   = []
        , deferred = []
        }


prettyModuleState :: ModuleState -> IO ()
prettyModuleState moduleState = do
    forM_ (Map.toList $ moduleDefs moduleState) $ \(name, def) -> do
        putStr (name ++ ": ")
        case def of
            Function instructions -> do
                putStrLn "function"
                forM_ instructions $ \i -> putStrLn ("\t" ++ show i)
            Variable _ _ -> putStrLn (show def)


newtype InstrT m a
    = InstrT { getInstr :: StateT InstrState m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState InstrState)


newtype ModuleT m a
    = ModuleT { getModule :: StateT ModuleState m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState ModuleState)


class (MonadState ModuleState m) => MonadModule m
class (MonadState InstrState m) => MonadInstr m


instance (Monad m) => MonadModule (ModuleT m)
instance (Monad m) => MonadInstr (InstrT m)


data Instruction
    = Store Name Value
    | If Value [Instruction] [Instruction]
    | Assert String
    deriving Show


data Definition
    = Function [Instruction]
    | Variable T.Type (Maybe Constant)
    deriving Show


data Constant
    = Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | String String
    deriving Show


data Value
    = Cons Constant
    deriving Show


isCons :: Value -> Bool
isCons (Cons _) = True


toCons :: Value -> Constant
toCons (Cons c) = c


cmpAST :: S.AST -> IO ModuleState
cmpAST ast = execStateT (getModule cmp) initModuleState
    where
        cmp :: ModuleT IO ()
        cmp = do
            name <- fresh "main"
            function name (mapM_ cmpTopStmt ast)


fresh :: MonadModule m => Symbol -> m Name
fresh symbol = do
    symbols <- gets symbolUsage
    let i = maybe 0 (+1) (Map.lookup symbol symbols)
    modify $ \s -> s { symbolUsage = Map.insert symbol i symbols }
    return (symbol ++ "_" ++ show i)


addDef :: MonadModule m => Name -> Definition -> m ()
addDef name def =
    modify $ \s -> s { moduleDefs = Map.insert name def (moduleDefs s) }


addInstr :: Monad m => Instruction -> InstrT m ()
addInstr instr =
    modify $ \s -> s { instrs = (instrs s) ++ [instr] }
    

function :: MonadModule m => Name -> InstrT m () -> m ()
function name cmp = do
    ins <- fmap instrs $ execStateT (getInstr cmp) initInstrState
    addDef name (Function ins)
    -- check exit


cmpIf :: MonadModule m => Value -> InstrT m () -> InstrT m () -> InstrT m ()
cmpIf (Cons (Bool True)) true _   = true
cmpIf (Cons (Bool False)) _ false = false
cmpIf cnd true false              = do
    trueInstr <- lift $ execStateT (getInstr true) initInstrState
    falseInstr <- lift $ execStateT (getInstr false) initInstrState 
    addInstr $ If cnd (instrs trueInstr) (instrs falseInstr)
    

cmpTopPattern :: MonadModule m => S.Pattern -> Value -> InstrT m Value
cmpTopPattern pattern val = case pattern of
    S.PatIgnore pos    -> return $ Cons (Bool True)
    S.PatIdent pos sym -> do
        name <- lift (fresh sym)
        if isCons val then
            lift $ addDef name $ Variable T.I64 (Just $ toCons val)
        else do
            lift $ addDef name (Variable T.I64 Nothing)
            addInstr (Store name val)
        return $ Cons (Bool True)
    

cmpTopStmt :: MonadModule m => S.Stmt -> InstrT m ()
cmpTopStmt stmt = case stmt of
    S.Assign pos pattern expr -> do
        val <- cmpExpr expr
        matched <- cmpTopPattern pattern val
        cmpIf matched (return ()) (addInstr $ Assert "pattern failure")


cmpExpr :: MonadModule m => S.Expr -> InstrT m Value
cmpExpr expr = case expr of
    S.Int pos n     -> return $ Cons (Int n)
    S.Float pos f   -> return $ Cons (Float f)
    S.Bool pos b    -> return $ Cons (Bool b)
    S.Char pos c    -> return $ Cons (Char c)
    S.String pos s  -> return $ Cons (String s)
    S.Ident pos sym -> do
        -- lookup symTab
        defs <- lift (gets moduleDefs)
        error "todo"
        return $ Cons (Int 4)
    S.Tuple pos exprs -> do
        vals <- mapM cmpExpr exprs
        error "todo"
        return $ Cons (Int 4)
