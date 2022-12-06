module States where

import qualified Data.Map as Map
import AST
import Symbol
import Type
import Interop


data ResolvedAst
    = ResolvedAst
    { moduleName  :: String
    , typeImports :: Map.Map Symbol AnnoType
    , funcImports :: Map.Map Symbol FuncKey
    , typeDefs    :: [Stmt]
    , funcDefs    :: [Stmt]
    }
    deriving (Eq)


type FuncKey = ([Type], String, [Type], Type)
data FuncBody = FuncBody
    { funcParams :: [AST.Param]
    , funcArgs   :: [AST.Param]
    , funcRetty  :: Type
    , funcStmts  :: [AST.Stmt]
    }


data IRGenState = IRGenState
    { irImports     :: [IRGenState]
    , irCExterns    :: [Extern]
    , irCurrentFunc :: FuncKey

    , irModuleName  :: String                      -- name of module
    , irTypeDefs    :: Map.Map Symbol AST.AnnoType -- all needed type definitions
    , irExternDefs  :: Map.Map Symbol FuncKey      -- all needed func declarations
    , irFuncDefs    :: Map.Map Symbol FuncBody     -- all needed func definitions
    , irFuncMap     :: Map.Map FuncKey Symbol      -- symbol table for funcs defined by this module
    , irTypeMap     :: Map.Map String Symbol       -- symbol table for types defined by this module
    , irMainDef     :: Maybe FuncBody              -- optional main() definition
    }

