module IR where

import qualified Data.Map as Map
import Control.Monad
import Data.List

import Type
import Symbol
import qualified AST as S


type ID = Int

data Constant
    = ConstBool Bool
    | ConstChar Char
    | ConstInt  Integer
    | ConstString String
    | ConstFloat Double

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
    deriving (Show)


data Arg
    = ArgConst Type Constant
    | ArgID ID

instance Show Arg where
    show (ArgConst typ const) = show const ++ ":" ++ show typ
    show (ArgID id)           = "%" ++ show id



data Stmt
    = SSA Int
    | Block [ID]
    | Loop [ID]
    | Break
    | Return Arg
    | ReturnVoid
    | EmbedC [ID] String
    | If Arg [ID]
    | Else [ID]

    -- value stmts
    | InitVar (Maybe Arg)
    | Call Type [Arg]
    | MakeReferenceFromValue ID
    | MakeValueFromReference ID
    | MakeFieldFromRef ID Int
    | MakeFieldFromVal ID Int
    | MakeString String
    deriving (Show)

showStmt :: Stmt -> String
showStmt statement = case statement of
    InitVar marg -> "init " ++ maybe "" show marg
    MakeReferenceFromValue id -> "&" ++ show (ArgID id)
    MakeValueFromReference id -> "*" ++ show (ArgID id)
    MakeFieldFromRef id n     -> show (ArgID id) ++ "->" ++ show n
    MakeFieldFromVal id n     -> show (ArgID id) ++ "." ++ show n
    MakeString str            -> show str
    Call callType args -> show callType ++ "(" ++ intercalate ", " (map show args) ++ ")"
    x -> error (show x)


data ParamIR
    = ParamIR Arg RefType Type

instance Show ParamIR where
    show (ParamIR arg refType typ) = show arg ++ " " ++ show refType ++ " " ++ show typ


data RettyIR
    = RettyIR RefType Type

instance Show RettyIR where
    show (RettyIR refType typ) = "(" ++ show refType ++ " " ++ show typ ++ ")"

data FuncIR = FuncIR
    { irHeader    :: S.FuncHeader
    , irStatement :: S.Stmt

    , irRetty     :: RettyIR
    , irArgs      :: [ParamIR]
    , irStmts     :: Map.Map ID Stmt
    , irTypes     :: Map.Map ID (Type, RefType)
    , irSymbols   :: Map.Map Symbol ID
    }


prettyIR :: String -> FuncIR -> IO ()
prettyIR pre funcIr = do

    putStrLn $ pre ++ "fn (" ++ intercalate ", " (map show $ irArgs funcIr) ++ ") " ++ show (irRetty funcIr)

    let Block ids = irStmts funcIr Map.! 0
    forM_ ids $ \id -> do
        prettyIrStmt (pre ++ "\t") funcIr id

    putStrLn ""



prettyIrStmt :: String -> FuncIR -> ID -> IO ()
prettyIrStmt pre funcIr id = do
    let resm = Map.lookup id (irTypes funcIr)
    case resm of
        Nothing -> putStr pre
        Just (typ, refType) -> do
            putStr $ pre ++ "%" ++ show id ++ " " ++ show refType ++ " " ++ show typ ++ " = "

        x -> error (show x)


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



        stmt-> putStrLn $ showStmt stmt

            
        x -> error (show x)


