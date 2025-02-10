{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ir where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.List
import Data.Maybe

import Type
import Symbol
import Error


type ID = Int


data Constant
    = ConstZero
    | ConstBool   Bool
    | ConstChar   Char
    | ConstInt    Integer
    | ConstString String
    | ConstFloat  Double
    | ConstTuple  [Constant]
    deriving (Eq)


instance Show Constant where
    show constant = case constant of
        ConstZero   -> "{0}"
        ConstBool b -> if b then "true" else "false"
        ConstChar c -> show c
        ConstInt  i -> show i
        ConstString s -> show s
        ConstFloat d -> show d
        ConstTuple cs -> "(" ++ intercalate ", " (map show cs) ++ ")"


data ModType
    = Modify
    | NonMod
    deriving (Eq, Show)


data Arg
    = ArgConst  { argType :: Type, argConst :: Constant }
    | ArgValue  { argType :: Type, argId :: ID }
    | ArgRef    { argType :: Type, argMod :: ModType, argId :: ID }
    | ArgSlice  { argType :: Type, argMod :: ModType, argId :: ID }
    deriving (Eq)


instance Show Arg where
    show (ArgConst typ const)  = show const
    show (ArgValue typ id)     = "%" ++ show id
    show (ArgRef typ mod id)   = "*" ++ show id
    show (ArgSlice typ mod id) = "[]" ++ show id


instance Typeof Arg where
    typeof = argType


data Stmt
    = Block [ID]
    | Loop [ID]
    | If ID [ID]
    | With [Arg] [ID]
    | Break
    | Return (Maybe ID)
    | EmbedC [(String, ID)] String
    | Call Arg Type [Arg]
    | MakeSlice Type [Arg]
    | Param Arg
    deriving (Eq)


instance Show Stmt where
    show stmt = case stmt of
        Call _ typ args -> show typ ++ "(" ++ intercalate ", " (map show args) ++ ")"
        MakeSlice typ args -> show typ ++ show args
        Block ids     -> "block" 
        Loop ids      -> "loop"
        If id ids     -> "if"
        Break         -> "break"
        Return mid    -> "return"
        EmbedC map st -> "embedC"
        With _ _      -> "with"
        Param _       -> "param"
        

data FuncIr = FuncIr
    { irStmts     :: Map.Map ID Stmt
    , irContexts  :: Maybe (Map.Map Type ID)
    , irArgs      :: [Arg]
    , irReturn    :: Arg
    , irSymbol    :: Symbol
    , irIdSupply  :: ID
    , irCurrentId :: ID
    , irTextPos   :: Map.Map ID TextPos
    }
    deriving (Eq)


initFuncIr = FuncIr
    { irStmts     = Map.singleton 0 (Block [])
    , irTextPos   = Map.empty
    , irReturn    = ArgValue Tuple undefined
    , irContexts  = Nothing
    , irSymbol    = Sym []
    , irIdSupply  = 1
    , irCurrentId = 0
    , irArgs      = []
    }



newtype FuncIrMonad a = FuncIrMonad
    { unFuncIrMonad :: StateT FuncIr (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState FuncIr, MonadError String)


instance MonadFail FuncIrMonad where
    fail = throwError


instance MonadFuncIr FuncIrMonad where
    liftFuncIrState (StateT s) = FuncIrMonad $ state (runIdentity . s)


class (Monad m, MonadFail m) => MonadFuncIr m where
    liftFuncIrState :: State FuncIr a -> m a


runFuncIrMonad :: FuncIr -> FuncIrMonad a -> Either String (a, FuncIr)
runFuncIrMonad funcIr f
     = runExcept $ runStateT (unFuncIrMonad f) funcIr


withCurrentId :: MonadFuncIr m => ID -> m a -> m a
withCurrentId id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }
    return a


generateId :: MonadFuncIr m => m ID
generateId = do
    id <- liftFuncIrState $ gets irIdSupply
    liftFuncIrState $ modify $ \s -> s { irIdSupply = (irIdSupply s) + 1 }
    return id


addTextPos :: MonadFuncIr m => ID -> TextPos -> m ()
addTextPos id pos = do
    liftFuncIrState $ modify $ \s -> s { irTextPos = Map.insert id pos (irTextPos s) }


getArg :: MonadFuncIr m => ID -> m Arg
getArg id = do
    stmt <- getStmt id
    case stmt of
        Just (Call retArg _ _) -> return retArg
        Just (Param arg)       -> return arg
        x -> error (show x)
        


addStmt :: MonadFuncIr m => ID -> Stmt -> m ()
addStmt id stmt = do
    resm <- liftFuncIrState $ gets $ Map.lookup id . irStmts
    unless (isNothing resm) (fail $ "stmt already added: " ++ show (id, stmt))
    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert id stmt (irStmts s) }


getStmt :: MonadFuncIr m => ID -> m (Maybe Stmt)
getStmt id = do 
    liftFuncIrState $ gets $ Map.lookup id . irStmts


appendStmt :: MonadFuncIr m => Stmt -> m ID
appendStmt stmt = do
    id <- generateId
    appendStmtWithId id stmt


appendStmtWithId :: MonadFuncIr m => ID -> Stmt -> m ID
appendStmtWithId id stmt = do
    curId <- liftFuncIrState $ (gets irCurrentId)
    curStmt <- liftFuncIrState $ gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        If arg ids -> return (If arg $ ids ++ [id])
        With args ids -> return (With args $ ids ++ [id])
        x -> error (show x)

    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    addStmt id stmt
    return id


prependStmt :: MonadFuncIr m => Stmt -> m ID
prependStmt stmt = do
    id <- generateId

    curId <- liftFuncIrState $ (gets irCurrentId)
    curStmt <- liftFuncIrState $ gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ (id : xs))
        Loop ids -> return (Loop $ (id : ids))
        If arg ids -> return (If arg $ (id : ids))
        With args ids -> return (With args $ id : ids)
        x -> error (show x)

    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    addStmt id stmt
    return id


getCurrentId :: MonadFuncIr m => m ID
getCurrentId = do
    liftFuncIrState $ gets irCurrentId


prettyFuncIr :: String -> FuncIr -> IO ()
prettyFuncIr pre funcIr = do
    let Block ids = irStmts funcIr Map.! 0
    forM_ ids $ \id -> do
        prettyIrStmt pre funcIr id


prettyIrStmt :: String -> FuncIr -> ID -> IO ()
prettyIrStmt pre funcIr id = case irStmts funcIr Map.! id of
    Block ids -> do
        putStrLn $ pre ++ "block: "
        forM_ ids $ prettyIrStmt (pre ++ "\t") funcIr

    EmbedC idMap str -> do
        putStrLn $ pre ++ "embedC " ++ intercalate ", " (map show idMap) ++ ": " ++ str

    Return marg -> do
        putStrLn $ pre ++ "return " ++ maybe "" show marg

    Loop ids -> do
        putStrLn $ pre ++ "loop:"
        mapM_ (prettyIrStmt (pre ++ "\t") funcIr) ids

    Break -> putStrLn $ pre ++ "break"

    If arg ids -> do
        putStrLn $ pre ++ "if " ++ show arg ++ ":"
        forM_ ids $ prettyIrStmt (pre ++ "\t") funcIr

    With args ids -> do
        putStrLn $ pre ++ "with " ++ show args ++ ":"
        forM_ ids $ prettyIrStmt (pre ++ "\t") funcIr

    Call retArg callType args -> 
        putStrLn $ pre ++ show id ++ " = " ++ show (Call retArg callType args)

    MakeSlice typ args -> do
        putStrLn $ pre ++ show id ++ " = " ++ "makeSlice:" ++ show typ ++ show args
        

    x -> error (show x)
        


