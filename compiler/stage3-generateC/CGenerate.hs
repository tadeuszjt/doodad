{-# LANGUAGE FlexibleContexts #-}
module CGenerate where

import qualified Data.Map as Map

import Monad
import Control.Monad.State


data ID =
    ID Int
    deriving (Show, Eq, Ord)


data Element
    = Global { globalBody :: [ID] }
    | For { forBody :: [ID] }
    | Func
        { funcBody :: [ID]
        , funcName :: String
        }


data CGenerateState
    = CGenerateState
    { elements :: Map.Map ID Element
    , curElement :: ID
    , idSupply :: Int
    }

initCGenerateState = CGenerateState
    { elements = Map.singleton (ID 0) (Global [])
    , curElement = ID 0
    , idSupply = 1
    }


freshId :: BoM CGenerateState m => m ID
freshId = do
    supply <- gets idSupply
    modify $ \s -> s { idSupply = supply + 1 }
    return (ID supply)


addElement :: BoM CGenerateState m => ID -> Element -> m ()
addElement id elem = modify $ \s -> s { elements = Map.insert id elem (elements s) }


newFunction :: BoM CGenerateState m => String -> m ID 
newFunction name = do
    id <- freshId
    addElement id (Func { funcName = name, funcBody = [] })
    return id
