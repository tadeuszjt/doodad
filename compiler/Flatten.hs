{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State 
import qualified Data.Set as Set 
import qualified Data.Map as Map 
import Data.List
import Type
import Monad
import Error
import Symbol

-- Flatten:
-- Check type defs for circles
-- Rearrage AST to put typedefs before funcdefs


-- check typedefs for circles
checkTypeDefs :: BoM s m => Map.Map Symbol Type -> m ()
checkTypeDefs typedefs = do
    -- check multiple definitions
    forM (Map.toList typedefs) $ \(symbol, anno) -> 
        case Map.lookup symbol typedefs of
            Just anno -> return ()
            Nothing   -> fail $ "multiple definitions of " ++ show symbol

    -- check circles
    mapM_ (checkCircles Set.empty) (Map.keys typedefs)

    where
        checkCircles :: BoM s m => Set.Set Symbol -> Symbol -> m ()
        checkCircles visited symbol = case Map.lookup symbol typedefs of
            Nothing -> return ()
            Just anno -> do
                assert (not $ Set.member symbol visited) "Typedef has circles"
                checkTypeCircles (Set.insert symbol visited) anno
        
        checkTypeCircles :: BoM s m => Set.Set Symbol -> Type -> m ()
        checkTypeCircles visited typ = case typ of
            Typedef symbol  -> checkCircles visited symbol
            Tuple ts        -> mapM_ (checkTypeCircles visited) ts
            Table ts        -> mapM_ (checkTypeCircles visited) ts
            Sparse ts       -> mapM_ (checkTypeCircles visited) ts
            ADT fs          -> do
                forM_ fs $ \field -> case field of
                    FieldNull -> return ()
                    FieldType t -> checkTypeCircles visited t
                    FieldCtor ts -> mapM_ (checkTypeCircles visited) ts
            Array n t       -> checkTypeCircles visited t
            Void            -> return ()
            t | isSimple t  -> return ()
            _                 -> fail ("checkTypeCircles " ++ show typ)
