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
        , typeImports :: Map.Map Symbol Type        -- imported types
        , ctorImports :: Map.Map Symbol (Type, Int) -- imported ctors
        , funcImports :: Map.Map Symbol FuncKey     -- imported funcs
        , constDefs   :: Map.Map Symbol Expr        -- defined consts
        , typeDefs    :: Map.Map Symbol Type        -- defined types
        , ctorDefs    :: Map.Map Symbol (Type, Int) -- defined ctors
        , funcDefs    :: Map.Map Symbol FuncBody    -- defined functions
        }
    deriving (Eq)


initASTResolved moduleName = ASTResolved
    { moduleName  = moduleName
    , includes    = Set.empty
    , links       = Set.empty
    , typeImports = Map.empty
    , ctorImports = Map.empty
    , funcImports = Map.empty
    , constDefs   = Map.empty
    , typeDefs    = Map.empty
    , ctorDefs    = Map.empty
    , funcDefs    = Map.empty
    }


type FuncKey = ([Type], String, [Type], Type)
data FuncBody
    = FuncBodyEmpty
    | FuncBody
        { funcParams :: [AST.Param]
        , funcArgs   :: [AST.Param]
        , funcRetty  :: Type
        , funcStmts  :: [AST.Stmt]
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
        prettyStmt "" (AST.Typedef undefined symbol $ AnnoType typ)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> 
        prettyStmt "" $
            FuncDef undefined (funcParams body) symbol (funcArgs body) (funcRetty body) (AST.Block $ funcStmts body)

    putStrLn ""
