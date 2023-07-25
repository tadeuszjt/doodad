module ASTResolved where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import AST
import Symbol
import Type


data ASTResolved
    = ASTResolved
        { moduleName  :: String
        , includes    :: Set.Set String             -- c header includes
        , links       :: Set.Set String             -- linked libraries
        , constDefs   :: Map.Map Symbol Expr        -- defined consts
        , typeDefs    :: Map.Map Symbol Type        -- defined types
        , ctorDefs    :: Map.Map Symbol (Type, Int) -- defined ctors
        , funcImports :: Map.Map Symbol FuncKey     -- imported funcs
        , funcDefs    :: Map.Map Symbol FuncBody    -- defined functions
        , symSupply   :: Map.Map String Int         -- type supply from resovle
        }
    deriving (Eq)


type FuncKey = ([Type], String, [Type], Type)
data FuncBody
    = FuncBodyEmpty
    | FuncBody
        { funcGenerics :: [Symbol]
        , funcParams   :: [AST.Param]
        , funcArgs     :: [AST.Param]
        , funcRetty    :: Type
        , funcStmt     :: AST.Stmt
        }
    deriving (Eq, Show)


funcKeyFromBody :: String -> FuncBody -> FuncKey
funcKeyFromBody sym body =
    (map typeof (funcParams body), sym, map typeof (funcArgs body), funcRetty body)


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module: " ++ moduleName ast

    forM_ (Map.toList $ constDefs ast) $ \(symbol, expr) ->
        prettyStmt "" $ AST.Const undefined symbol expr

    forM_ (Map.toList $ typeDefs ast) $ \(symbol, typ) -> 
        prettyStmt "" (AST.Typedef undefined [] symbol $ AnnoType typ)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> 
        prettyStmt "" $ FuncDef
            undefined
            (funcGenerics body)
            (funcParams body)
            symbol
            (funcArgs body)
            (funcRetty body)
            (funcStmt body)

    putStrLn ""
