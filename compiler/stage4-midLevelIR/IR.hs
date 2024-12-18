module IR where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe

import Type
import Symbol
import Error


type ID = Int

data Constant
    = ConstBool Bool
    | ConstChar Char
    | ConstInt  Integer
    | ConstString String
    | ConstFloat Double
    | ConstTuple [Constant]
    deriving (Eq)

instance Show Constant where
    show constant = case constant of
        ConstBool b -> if b then "true" else "false"
        ConstChar c -> show c
        ConstInt  i -> show i
        ConstString s -> show s
        ConstFloat d -> show d
        ConstTuple cs -> "(" ++ intercalate ", " (map show cs) ++ ")"



data RefType
    = Const  -- cant be addressed
    | Value  -- can be addressed
    | Ref    -- user ref (&)
    | Slice  -- user slice ([])
    deriving (Show, Eq)


data Arg
    = ArgConst Type Constant
    | ArgID ID
    deriving (Eq)

instance Show Arg where
    show (ArgConst typ const) = show const ++ ":" ++ show typ
    show (ArgID id)           = "%" ++ show id


data Operation
    = InitVar (Maybe Arg)
    | Call Type [Arg]
    | MakeReferenceFromValue Arg
    | MakeValueFromReference Arg
    | MakeString String
    | MakeSlice [Arg]
    deriving (Eq)

instance Show Operation where
    show operation = case operation of
        InitVar marg -> "init " ++ maybe "" show marg
        MakeReferenceFromValue arg -> "&" ++ show arg
        MakeValueFromReference arg -> "*" ++ show arg
        MakeString str            -> show str
        MakeSlice args -> "[" ++ intercalate ", " (map show args) ++ "]"
        Call callType args -> show callType ++ "(" ++ intercalate ", " (map show args) ++ ")"
        x -> error (show x)

data Stmt
    = Block [ID]
    | Loop [ID]
    | Break
    | Return Arg
    | EmbedC [(String, ID)] String
    | If Arg ID ID
    | SSA Operation
    deriving (Show, Eq)


data ParamIR
    = ParamIR RefType Type
    deriving (Eq)

instance Show ParamIR where
    show (ParamIR refType typ) = show refType ++ " " ++ show typ


data RettyIR
    = RettyIR RefType Type
    deriving (Eq)

instance Show RettyIR where
    show (RettyIR refType typ) = "(" ++ show refType ++ " " ++ show typ ++ ")"


data FuncIrHeader = FuncIrHeader
    { irRetty     :: RettyIR
    , irArgs      :: [ParamIR]
    , irFuncSymbol :: Symbol
    }
    deriving (Eq)

instance Show FuncIrHeader where
    show (FuncIrHeader retty args symbol) = prettySymbol symbol ++ " (" ++ intercalate ", " (map show args) ++ ") " ++ show retty


data FuncIR = FuncIR
    { irStmts     :: Map.Map ID Stmt
    , irTypes     :: Map.Map ID (Type, RefType)
    , irIdSupply  :: ID
    , irCurrentId :: ID
    , irTextPos   :: Map.Map ID TextPos
    }
    deriving (Eq)


initFuncIr = FuncIR
    { irStmts     = Map.singleton 0 (Block [])
    , irTypes     = Map.empty
    , irTextPos   = Map.empty
    , irIdSupply  = 1
    , irCurrentId = 0
    }

class (Monad m, MonadFail m) => MonadFuncIR m where
    liftFuncIrState :: State FuncIR a -> m a


withCurrentId :: MonadFuncIR m => ID -> m a -> m a
withCurrentId id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }
    return a


generateId :: MonadFuncIR m => m ID
generateId = do
    id <- liftFuncIrState $ gets irIdSupply
    liftFuncIrState $ modify $ \s -> s { irIdSupply = (irIdSupply s) + 1 }
    return id


addType :: MonadFuncIR m => ID -> Type -> RefType -> m ()
addType id typ refType = do
    resm <- liftFuncIrState $ gets (Map.lookup id . irTypes)
    unless (isNothing resm) (fail $ "id already typed: " ++ show id)
    liftFuncIrState $ modify $ \s -> s { irTypes = Map.insert id (typ, refType) (irTypes s) }


addTextPos :: MonadFuncIR m => ID -> TextPos -> m ()
addTextPos id pos = do
    liftFuncIrState $ modify $ \s -> s { irTextPos = Map.insert id pos (irTextPos s) }


addStmt :: MonadFuncIR m => ID -> Stmt -> m ()
addStmt id stmt = do
    resm <- liftFuncIrState $ gets $ Map.lookup id . irStmts
    unless (isNothing resm) (fail $ "stmt already added: " ++ show (id, stmt))
    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert id stmt (irStmts s) }


getType :: MonadFuncIR m => Arg -> m (Type, RefType)
getType arg = case arg of
    ArgConst typ _ -> return (typ, Const)
    ArgID id -> do
        resm <- liftFuncIrState $ gets (Map.lookup id . irTypes)
        unless (isJust resm) (fail $ "id isn't typed: " ++ show id)
        return (fromJust resm)
    

appendStmt :: MonadFuncIR m => Stmt -> m ID
appendStmt stmt = do
    id <- generateId

    curId <- liftFuncIrState $ (gets irCurrentId)
    curStmt <- liftFuncIrState $ gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        x -> error (show x)

    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    addStmt id stmt
    return id


appendSSA :: MonadFuncIR m => Type -> RefType -> Operation -> m ID
appendSSA typ refType op = do
    id <- appendStmt (SSA op)
    addType id typ refType
    return id


appendStmtWithId :: MonadFuncIR m => ID -> Stmt -> m ()
appendStmtWithId id stmt = do
    curId <- liftFuncIrState (gets irCurrentId)
    curStmt <- liftFuncIrState $ gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        x -> error (show x)

    liftFuncIrState $ modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    addStmt id stmt


prettyIR :: String -> FuncIR -> IO ()
prettyIR pre funcIr = do
    --putStrLn $ pre ++ "fn (" ++ intercalate ", " (map show $ irArgs funcIr) ++ ") " ++ show (irRetty funcIr)

    let Block ids = irStmts funcIr Map.! 0
    forM_ ids $ \id -> do
        prettyIrStmt (pre ++ "\t") funcIr id

    putStrLn ""



prettyIrStmt :: String -> FuncIR -> ID -> IO ()
prettyIrStmt pre funcIr id = do
    putStr pre
    case irStmts funcIr Map.! id of
        Block ids -> do
            putStrLn $ "block: "
            forM_ ids $ \id -> prettyIrStmt (pre ++ "\t") funcIr id

        EmbedC idMap str -> do
            putStrLn $ "embedC " ++ intercalate ", " (map show idMap) ++ ": " ++ str

        Return arg -> do
            putStrLn $ "return " ++ show arg

        Loop ids -> do
            putStrLn $ "loop:"
            mapM_ (prettyIrStmt (pre ++ "\t") funcIr) ids

        Break -> putStrLn "break"

        If arg trueId falseId -> do
            putStr $ "if " ++ show arg ++ ":"
            prettyIrStmt pre funcIr trueId
            putStr $ pre ++ "else:"
            prettyIrStmt pre funcIr falseId

        SSA operation -> do
            let Just (typ, refType) = Map.lookup id (irTypes funcIr)
            putStrLn $ "%" ++ show id ++ " " ++ show refType ++ " " ++ show typ ++ " = " ++ show operation
            
        x -> error (show x)


