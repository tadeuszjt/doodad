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
    , typeImports :: Map.Map Symbol AnnoType -- imported types
    , funcImports :: Map.Map Symbol FuncKey  -- imported funcs
    , typeDefsMap :: Map.Map Symbol AnnoType -- defined types
    , funcDefs    :: Map.Map Symbol FuncBody -- defined functions
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
    deriving (Eq)


data IRGenState = IRGenState
    { irCurrentFunc :: FuncKey

    , irModuleName  :: String                      -- name of module
    , irTypeDefs    :: Map.Map Symbol AST.AnnoType -- all needed type definitions
    , irExternDefs  :: Map.Map Symbol FuncKey      -- all needed func declarations
    , irFuncDefs    :: Map.Map Symbol FuncBody     -- all needed func definitions
    , irFuncMap     :: Map.Map FuncKey Symbol      -- symbol table for funcs defined by this module
    , irTypeMap     :: Map.Map String Symbol       -- symbol table for types defined by this module
    , irMainDef     :: Maybe FuncBody              -- optional main() definition
    }


prettyResolvedAst :: ResolvedAst -> IO ()
prettyResolvedAst ast = do
    putStrLn $ "module: " ++ moduleName ast
    forM_ (Map.toList $ typeImports ast) $ \(symbol, anno) -> 
        putStrLn $ "type import: " ++ show symbol ++ " " ++ show anno
    forM_ (Map.toList $ funcImports ast) $ \(symbol, key) -> 
        putStrLn $ "func import: " ++ show symbol ++ " " ++ show key

    putStrLn ""
