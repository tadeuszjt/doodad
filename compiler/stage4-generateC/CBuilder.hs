module CBuilder where

import qualified Data.Map as Map

import CAst
import Control.Monad.State
import Error


data BuilderState
    = BuilderState
    { elements :: Map.Map ID Element
    , currentID :: ID
    , idSupply :: Int
    , moduleName :: String
    }
    deriving (Eq)


class (Monad m, MonadFail m) => MonadBuilder m where
    liftBuilderState :: State BuilderState a -> m a


globalID = ID 0


initBuilderState moduleName = BuilderState
    { elements = Map.singleton globalID (Global [])
    , currentID = globalID
    , idSupply = 1
    , moduleName = moduleName
    }


freshId :: MonadBuilder m => m ID
freshId = do
    supply <- liftBuilderState $ gets idSupply
    liftBuilderState $ modify $ \s -> s { idSupply = supply + 1 }
    return (ID supply)


withCurID :: MonadBuilder m => ID -> m a -> m a
withCurID id f = do
    curId <- liftBuilderState $ gets currentID
    setCurrentId id
    a <- f
    setCurrentId curId
    return a


setCurrentId :: MonadBuilder m => ID -> m ()
setCurrentId id = do
    liftBuilderState $ modify $ \s -> s { currentID = id }


append :: MonadBuilder m => ID -> m ()
append id = do
    curId <- liftBuilderState $ gets currentID
    curElem <- mapGet curId =<< liftBuilderState (gets elements)

    elem' <- case curElem of
        global@(Global _) -> return $ global { globalBody = globalBody global ++ [id] }
        func@(Func _ _ _ _ _) -> return $ func { funcBody = funcBody func ++ [id] }
        if_@(If _ _) -> return $ if_ { ifStmts = ifStmts if_ ++ [id] }
        els@(Else _) -> return $ els { elseStmts = elseStmts els ++ [id] }
        switch@(Switch _ _) -> return $ switch { switchBody = switchBody switch ++ [id] }
        cas@(Case _ _) -> return $ cas { caseBody = caseBody cas ++ [id] }
        for@(For _ _ _ _) -> return $ for { forBody = forBody for ++ [id] }

    liftBuilderState $ modify $ \s -> s { elements = Map.insert curId elem' (elements s) }


appendElem :: MonadBuilder m => Element -> m ID
appendElem elem = do
    id <- newElement elem
    append id
    return id


appendIf :: MonadBuilder m => Expression -> m ID
appendIf cnd = do
    id <- newElement $ If { ifExpr = cnd, ifStmts = [] }
    append id
    return id


appendElse :: MonadBuilder m => m ID
appendElse = do
    appendElem $ Else { elseStmts = [] }


appendPrintf :: MonadBuilder m => String -> [Expression] -> m ID
appendPrintf fmt exprs = do
    appendElem $ ExprStmt $ Call "printf" (String fmt : exprs)


appendAssign :: MonadBuilder m => Type -> String -> Expression -> m ID
appendAssign ctyp name expr = do
    appendElem $ Assign ctyp name expr


newElement :: MonadBuilder m => Element -> m ID
newElement elem = do
    id <- freshId
    liftBuilderState $ modify $ \s -> s { elements = Map.insert id elem (elements s) }
    return id


modifyElement :: MonadBuilder m => ID -> (Element -> m Element) -> m ()
modifyElement id f = do
    elem' <- f =<< mapGet id =<< liftBuilderState (gets elements)
    liftBuilderState $ modify $ \s -> s { elements = Map.insert id elem' (elements s) }


newExtern :: MonadBuilder m => String -> Type -> [Type] -> [Qualifier] -> m ()
newExtern name retty args qualifiers = do
    id <- newElement $ ExternFunc
        { extName = name
        , extRetty = retty
        , extArgs = args
        , extQualifiers = qualifiers
        }
    withCurID globalID (append id)


newTypedef :: MonadBuilder m => Type -> String -> m ID
newTypedef typ name = do
    id <- newElement (Typedef { typedefName = name, typedefType = typ })
    withCurID globalID (append id)
    return id


newFunction :: MonadBuilder m => Type -> String -> [Param] -> [Qualifier] -> m ID 
newFunction retty name args qualifiers = do
    newElement $ Func
        { funcName = name
        , funcBody = []
        , funcRetty = retty
        , funcArgs = args
        , funcQualifiers = qualifiers
        }
