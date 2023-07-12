module States where

import Control.Monad
import qualified Data.Map as Map
import AST
import Symbol
import Type


data ResolvedAst
    = ResolvedAst
    { moduleName  :: String
    , typeImports :: Map.Map Symbol Type        -- imported types
    , ctorImports :: Map.Map Symbol (Type, Int) -- imported ctors
    , funcImports :: Map.Map Symbol FuncKey     -- imported funcs
    , typeDefs    :: Map.Map Symbol Type        -- defined types
    , ctorDefs    :: Map.Map Symbol (Type, Int) -- defined ctors
    , funcDefs    :: Map.Map Symbol FuncBody    -- defined functions
    }
    deriving (Eq)

initResolvedAst moduleName = ResolvedAst
    { moduleName = moduleName
    , typeImports = Map.empty
    , ctorImports  = Map.empty
    , funcImports  = Map.empty
    , typeDefs     = Map.empty
    , ctorDefs     = Map.empty
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
funcKeyFromBody sym body = (map typeof (funcParams body), sym, map typeof (funcArgs body), funcRetty body)


prettyResolvedAst :: ResolvedAst -> IO ()
prettyResolvedAst ast = do
    putStrLn $ "module: " ++ moduleName ast

    forM_ (Map.toList $ typeDefs ast) $ \(symbol, typ) -> 
        prettyStmt "" (AST.Typedef undefined symbol $ AnnoType typ)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> 
        prettyStmt "" $
            FuncDef undefined (funcParams body) symbol (funcArgs body) (funcRetty body) (AST.Block $ funcStmts body)

--    forM_ (Map.toList $ typeImports ast) $ \(symbol, anno) -> 
--        putStrLn $ "type import: " ++ show symbol ++ " " ++ show anno
--    forM_ (Map.toList $ funcImports ast) $ \(symbol, key) -> 
--        putStrLn $ "func import: " ++ show symbol ++ " " ++ show key

    putStrLn ""
