module States where

import Control.Monad
import qualified Data.Map as Map
import AST
import Symbol
import Type


data Extern
    = ExtFunc String [Type] Type
    | ExtVar String AnnoType
    | ExtConstInt String Integer
    | ExtTypeDef String Type
    deriving (Show, Eq)


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


data IRGenState = IRGenState
    { irCurrentFunc :: FuncKey
    , irTupleFields :: Map.Map (Symbol, String) Symbol

    , irModuleName  :: String                      -- name of module
    , irTypeDefs    :: Map.Map Symbol Type         -- all needed type definitions
    , irExternDefs  :: Map.Map Symbol FuncKey      -- all needed func declarations
    , irFuncDefs    :: Map.Map Symbol FuncBody     -- all needed func definitions
    , irCtorDefs    :: Map.Map Symbol (Type, Int)  -- all needed ctor definitions
    , irFuncMap     :: Map.Map FuncKey Symbol      -- symbol table for funcs defined by this module
    , irTypeMap     :: Map.Map String Symbol       -- symbol table for types defined by this module
    , irCtorMap     :: Map.Map String Symbol       -- symbol table for ctors defined by this module
    , irMainDef     :: Maybe FuncBody              -- optional main() definition
    }


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
