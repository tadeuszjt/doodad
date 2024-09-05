module InferTypes where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import Type
import Monad
import InstBuilder
import ASTResolved
import Constraint


annotate :: InstBuilderState -> DoM Int InstBuilderState
annotate state = do
    types' <- forM (types state) (mapTypeM f)
    return state { types = types' }
    where
        f :: Type -> DoM Int Type
        f typ = case typ of
            Type 0 -> do
                n <- get
                modify (+1) 
                return (Type $ n + 1)
            _ -> return typ



deAnnotate :: InstBuilderState -> DoM () InstBuilderState
deAnnotate state = do
    types' <- forM (types state) (mapTypeM f)
    return state { types = types' }
    where
        f :: Type -> DoM () Type
        f typ = case typ of
            Type n -> return (Type 0)
            _ -> return typ


data CollectState = CollectState
    { collected :: [Constraint]
    , defaulted :: [Constraint]
    , astResolved :: ASTResolved
    }

initCollectState ast = CollectState
    { collected = []
    , defaulted = []
    , astResolved = ast
    }



collect :: InstBuilderState -> DoM CollectState ()
collect state = do
    forM_ (Map.toList $ expressions state) $ \(id, expression) -> case expression of
        
        x -> error (show x)

    forM_ (Map.toList $ patterns state) $ \(id, pattern) -> case pattern of
        x -> error (show x)
    
    forM_ (Map.toList $ statements state) $ \(id, statement) -> case statement of
        x -> error (show x)



apply :: [(Type, Type)] -> InstBuilderState -> DoM () InstBuilderState
apply subs state = do
    types' <- forM (types state) (return . applyType subs)
    return $ state { types = types' }
    
    
