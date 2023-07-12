{-# LANGUAGE FlexibleContexts #-}
module CBuilder where

import qualified Data.Map as Map

import Monad
import Control.Monad.State


data ID =
    ID Int
    deriving (Show, Eq, Ord)


data CParam
    = CParam { cName :: String, cType :: CType }
    deriving (Eq)

instance Show CParam where
    show (CParam name typ) = show typ ++ " " ++ name

data CType
    = Cint
    | Cfloat
    | Cdouble
    | Cint64_t
    | Cint32_t
    | Cint16_t
    | Cint8_t
    | Cuint64_t
    | Cuint32_t
    | Cbool
    | Cchar
    | Cstruct [CParam]
    | Cunion [CParam]
    | Cvoid
    | Ctypedef String
    | Cpointer CType
    deriving (Eq)


instance Show CType where
    show Cint = "int"
    show Cfloat = "float"
    show Cdouble = "double"
    show Cint8_t = "int8_t"
    show Cint16_t = "int16_t"
    show Cint32_t = "int32_t"
    show Cint64_t = "int64_t"
    show Cbool = "bool"
    show Cchar = "char"
    show (Cstruct ts) = "struct { " ++ concat (map (\t -> show t ++ "; ") ts) ++ "}"
    show (Cunion ts) = "union { " ++ concat (map (\t -> show t ++ "; ") ts) ++ "}"
    show Cvoid = "void"
    show (Ctypedef s) = s
    show (Cpointer t) = show t ++ "*"


data Element
    = Global { globalBody :: [ID] }
    | For { forBody :: [ID] }
    | Extern
        { extName :: String
        , extRetty :: CType
        , extArgs :: [CType]
        }
    | Func
        { funcBody :: [ID]
        , funcName :: String
        , funcArgs :: [CParam]
        , funcRetty :: CType
        }
    | Typedef
        { typedefName :: String
        , typedefType :: CType
        }


data CBuilderState
    = CBuilderState
    { elements :: Map.Map ID Element
    , curElement :: ID
    , idSupply :: Int
    , moduleName :: String
    }

initCBuilderState moduleName = CBuilderState
    { elements = Map.singleton (ID 0) (Global [])
    , curElement = ID 0
    , idSupply = 1
    , moduleName = moduleName
    }


freshId :: BoM CBuilderState m => m ID
freshId = do
    supply <- gets idSupply
    modify $ \s -> s { idSupply = supply + 1 }
    return (ID supply)


addElement :: BoM CBuilderState m => ID -> Element -> m ()
addElement id elem = modify $ \s -> s { elements = Map.insert id elem (elements s) }

modifyElement :: BoM CBuilderState m => ID -> (Element -> m Element) -> m ()
modifyElement id f = do
    elem' <- f =<< (Map.! id) <$> gets elements
    modify $ \s -> s { elements = Map.insert id elem' (elements s) }

addToGlobal :: BoM CBuilderState m => ID -> m ()
addToGlobal id = do
    modifyElement (ID 0) (\global -> return $ global { globalBody = globalBody global ++ [id] })

setCurElement :: BoM CBuilderState m => ID -> m ()
setCurElement id = do
    modify $ \s -> s { curElement = id }


newExtern :: BoM CBuilderState m => String -> CType -> [CType] -> m ()
newExtern name retty args = do
    id <- freshId
    addToGlobal id
    addElement id (Extern { extName = name, extRetty = retty, extArgs = args })


newTypedef :: BoM CBuilderState m => CType -> String -> m ID
newTypedef typ name = do
    id <- freshId
    addElement id (Typedef { typedefName = name, typedefType = typ })
    addToGlobal id
    return id

-- creates function, appends to global
-- sets current element to function
newFunction :: BoM CBuilderState m => CType -> String -> [CParam] -> m ID 
newFunction retty name args = do
    id <- freshId
    addElement id (Func { funcName = name, funcBody = [], funcRetty = retty, funcArgs = args })
    addToGlobal id
    setCurElement id
    return id
