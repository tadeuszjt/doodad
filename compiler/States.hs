module States where

import qualified Data.Map as Map
import AST
import Symbol
import Type
import Interop
import qualified IR


data ResolvedAst
    = ResolvedAst
    { moduleName :: String
    , typeDefs   :: [Stmt]
    , funcDefs   :: [Stmt]
    }
    deriving (Eq)



type FuncKey = ([Type], String, [Type], Type)
data FuncBody = FuncBody
    { funcParams :: [AST.Param]
    , funcArgs   :: [AST.Param]
    , funcRetty  :: Type
    , funcStmts  :: [IR.Stmt]
    }

data IRGenState
    = IRGenState
        { irImports     :: [IRGenState]
        , irCExterns    :: [Extern]

        , irModuleName  :: String
        , irTypeDefs    :: Map.Map Symbol AST.AnnoType
        , irExternDefs  :: Map.Map Symbol FuncKey
        , irFuncDefs    :: Map.Map Symbol FuncBody
        , irFuncMap     :: Map.Map FuncKey Symbol
        , irTypeMap     :: Map.Map String Symbol
        , irMainDef     :: Maybe FuncBody

        , irCurrentFunc :: FuncKey
        }

