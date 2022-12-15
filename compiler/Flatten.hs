{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State 
import qualified Data.Set as Set 
import qualified Data.Map as Map 
import Data.List
import qualified AST as S
import qualified Type as T
import Monad
import Error
import Symbol

-- Flatten:
-- Check type defs for circles
-- Rearrage AST to put typedefs before funcdefs


-- check typedefs for circles
checkTypeDefs :: BoM s m => Map.Map Symbol S.AnnoType -> m ()
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
                checkAnnoCircles (Set.insert symbol visited) anno
        
        checkAnnoCircles :: BoM s m => Set.Set Symbol -> S.AnnoType -> m ()
        checkAnnoCircles visited anno = case anno of
            S.AnnoType t   -> checkTypeCircles visited t
            S.AnnoTuple xs -> forM_ xs $ \(_, t) -> checkTypeCircles visited t
            S.AnnoADT xs   -> return () -- no need to check circles
            S.AnnoEnum ss  -> return ()

        checkTypeCircles :: BoM s m => Set.Set Symbol -> T.Type -> m ()
        checkTypeCircles visited typ = case typ of
            T.Typedef symbol  -> checkCircles visited symbol
            T.Tuple ts        -> mapM_ (checkTypeCircles visited) ts
            T.Table ts        -> mapM_ (checkTypeCircles visited) ts
            T.Sparse ts       -> mapM_ (checkTypeCircles visited) ts
            T.ADT fs          -> do
                forM_ fs $ \field -> case field of
                    T.FieldNull -> return ()
                    T.FieldType t -> checkTypeCircles visited t
                    T.FieldCtor ts -> mapM_ (checkTypeCircles visited) ts
            T.Array n t       -> checkTypeCircles visited t
            T.Void            -> return ()
            t | T.isSimple t  -> return ()
            _                 -> fail ("checkTypeCircles " ++ show typ)
