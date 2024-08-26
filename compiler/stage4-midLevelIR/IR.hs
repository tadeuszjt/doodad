module IR where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe

import Type
import qualified AST as S
import Monad
import Symbol


type ID = Int

data Constant
    = ConstBool Bool
    | ConstChar Char
    | ConstInt  Integer
    | ConstString String
    | ConstFloat Double
    deriving (Eq)

instance Show Constant where
    show constant = case constant of
        ConstBool b -> if b then "true" else "false"
        ConstChar c -> show c
        ConstInt  i -> show i
        ConstString s -> show s
        ConstFloat d -> show d



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
    | MakeReferenceFromValue ID
    | MakeValueFromReference ID
    | MakeValueFromValue Arg
    | MakeRefFromRef Arg
    | MakeString String
    deriving (Eq)

instance Show Operation where
    show operation = case operation of
        InitVar marg -> "init " ++ maybe "" show marg
        MakeReferenceFromValue id -> "&" ++ show (ArgID id)
        MakeValueFromReference id -> "*" ++ show (ArgID id)
        MakeValueFromValue arg    -> show arg
        MakeRefFromRef arg        -> show arg
        MakeString str            -> show str
        Call callType args -> show callType ++ "(" ++ intercalate ", " (map show args) ++ ")"
        x -> error (show x)

data Stmt
    = Block [ID]
    | Loop [ID]
    | Break
    | Return Arg
    | ReturnVoid
    | EmbedC [ID] String
    | If Arg [ID]
    | Else [ID]
    | SSA Type RefType Operation
    deriving (Show, Eq)


data ParamIR
    = ParamIR Arg RefType Type
    deriving (Eq)

instance Show ParamIR where
    show (ParamIR arg refType typ) = show arg ++ " " ++ show refType ++ " " ++ show typ


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
    }
    deriving (Eq)


initFuncIr = FuncIR
    { irStmts     = Map.singleton 0 (Block [])
    , irTypes     = Map.empty
    , irIdSupply  = 1
    , irCurrentId = 0
    }



generateId :: DoM FuncIR ID
generateId = do
    id <- gets irIdSupply
    modify $ \s -> s { irIdSupply = (irIdSupply s) + 1 }
    return id


addType :: ID -> Type -> RefType -> DoM FuncIR ()
addType id typ refType = do
    resm <- gets (Map.lookup id . irTypes)
    unless (isNothing resm) (fail $ "id already typed: " ++ show id)
    modify $ \s -> s { irTypes = Map.insert id (typ, refType) (irTypes s) }




getType :: Arg -> DoM FuncIR (Type, RefType)
getType arg = case arg of
    ArgConst typ _ -> return (typ, Const)
    ArgID id -> do
        resm <- gets (Map.lookup id . irTypes)
        unless (isJust resm) (fail $ "id isn't typed: " ++ show id)
        return (fromJust resm)
    

appendStmt :: Stmt -> DoM FuncIR ID
appendStmt stmt = do
    id <- generateId

    curId <- (gets irCurrentId)
    curStmt <- gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        If arg ids    -> return (If arg $ ids ++ [id])
        Else ids      -> return (Else $ ids ++ [id])
        x -> error (show x)

    modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    modify $ \s -> s { irStmts = Map.insert id stmt (irStmts s) }
    return id


appendSSA :: Type -> RefType -> Operation -> DoM FuncIR ID
appendSSA typ refType op = do
    id <- appendStmt $ SSA typ refType op
    addType id typ refType
    return id


appendStmtWithId :: ID -> Stmt -> DoM FuncIR ()
appendStmtWithId id stmt = do
    curId <- (gets irCurrentId)
    curStmt <- gets $ (Map.! curId) . irStmts
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        If arg ids    -> return (If arg $ ids ++ [id])
        Else ids      -> return (Else $ ids ++ [id])
        x -> error (show x)

    modify $ \s -> s { irStmts = Map.insert curId curStmt' (irStmts s) }
    modify $ \s -> s { irStmts = Map.insert id stmt (irStmts s) }


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

        EmbedC uses str -> do
            putStrLn $ "embedC " ++ intercalate ", " (map (show . ArgID) uses) ++ ": " ++ str

        Return arg -> do
            putStrLn $ "return " ++ show arg

        ReturnVoid -> putStrLn "return"

        Loop ids -> do
            putStrLn $ "loop:"
            mapM_ (prettyIrStmt (pre ++ "\t") funcIr) ids

        Break -> putStrLn "break"

        If arg trueIds -> do
            putStrLn $ "if " ++ show arg ++ ":"
            mapM_ (prettyIrStmt (pre ++ "\t") funcIr) trueIds

        Else ids -> do
            putStrLn $ "else:"
            mapM_ (prettyIrStmt (pre ++ "\t") funcIr) ids

        SSA typ refType operation -> do
            putStrLn $ "%" ++ show id ++ " " ++ show refType ++ " " ++ show typ ++ " = " ++ show operation
            
        x -> error (show x)


