{-# LANGUAGE FlexibleContexts #-}
module CBuilder where

import qualified Data.Map as Map

import Monad
import CAst
import Control.Monad.State


data ID =
    ID Int
    deriving (Show, Eq, Ord)

data Element
    = Global { globalBody :: [ID] }
    | For { forBody :: [ID] }
    | Extern
        { extName :: String
        , extRetty :: Type
        , extArgs :: [Type]
        }
    | Func
        { funcBody :: [ID]
        , funcName :: String
        , funcArgs :: [Param]
        , funcRetty :: Type
        }
    | Typedef
        { typedefName :: String
        , typedefType :: Type
        }
    | Return Expression
    | Break
    | Assign Type String Expression
    | If
        { ifExpr :: Expression
        , ifStmts :: [ID]
        }
    | Else
        { elseStmts :: [ID]
        }
    | ExprStmt Expression
    | Switch
        { switchExpr :: Expression
        , switchBody :: [ID]
        }
    | Case
        { caseExpr :: Expression
        , caseBody :: [ID]
        }
    deriving (Show)
 

data BuilderState
    = BuilderState
    { elements :: Map.Map ID Element
    , currentID :: ID
    , idSupply :: Int
    , moduleName :: String
    }


globalID = ID 0

initBuilderState moduleName = BuilderState
    { elements = Map.singleton globalID (Global [])
    , currentID = globalID
    , idSupply = 1
    , moduleName = moduleName
    }


freshId :: BoM BuilderState m => m ID
freshId = do
    supply <- gets idSupply
    modify $ \s -> s { idSupply = supply + 1 }
    return (ID supply)

withCurID :: BoM BuilderState m => ID -> m a -> m a
withCurID id f = do
    curId <- gets currentID
    setCurrentId id
    a <- f
    setCurrentId curId
    return a

setCurrentId :: BoM BuilderState m => ID -> m ()
setCurrentId id = do
    modify $ \s -> s { currentID = id }


append :: BoM BuilderState m => ID -> m ()
append id = do
    curId <- gets currentID
    curElem <- (Map.! curId) <$> gets elements

    elem' <- case curElem of
        global@(Global _) -> return $ global { globalBody = globalBody global ++ [id] }
        func@(Func _ _ _ _) -> return $ func { funcBody = funcBody func ++ [id] }
        if_@(If _ _) -> return $ if_ { ifStmts = ifStmts if_ ++ [id] }
        els@(Else _) -> return $ els { elseStmts = elseStmts els ++ [id] }
        switch@(Switch _ _) -> return $ switch { switchBody = switchBody switch ++ [id] }
        cas@(Case _ _) -> return $ cas { caseBody = caseBody cas ++ [id] }

    modify $ \s -> s { elements = Map.insert curId elem' (elements s) }


newElement :: BoM BuilderState m => Element -> m ID
newElement elem = do
    id <- freshId
    modify $ \s -> s { elements = Map.insert id elem (elements s) }
    return id


modifyElement :: BoM BuilderState m => ID -> (Element -> m Element) -> m ()
modifyElement id f = do
    elem' <- f =<< (Map.! id) <$> gets elements
    modify $ \s -> s { elements = Map.insert id elem' (elements s) }



newExtern :: BoM BuilderState m => String -> Type -> [Type] -> m ()
newExtern name retty args = do
    id <- newElement (Extern { extName = name, extRetty = retty, extArgs = args })
    withCurID globalID (append id)


newTypedef :: BoM BuilderState m => Type -> String -> m ID
newTypedef typ name = do
    id <- newElement (Typedef { typedefName = name, typedefType = typ })
    withCurID globalID (append id)
    return id

-- creates function, appends to global
-- sets current element to function
newFunction :: BoM BuilderState m => Type -> String -> [Param] -> m ID 
newFunction retty name args = do
    id <- newElement (Func { funcName = name, funcBody = [], funcRetty = retty, funcArgs = args })
    withCurID globalID (append id)
    return id
