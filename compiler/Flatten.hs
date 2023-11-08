module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import qualified Data.Map as Map 
import qualified Data.Set as Set
import Control.Monad
import Data.List
import Type
import Monad
import Error
import Symbol

-- Flatten:
-- check typedefs for circles
checkTypeDefs :: Map.Map Symbol Type -> DoM s ()
checkTypeDefs typedefs = do
    mapM_ (checkCircles Set.empty) (Map.keys typedefs)
    where
        checkCircles :: Set.Set Symbol -> Symbol -> DoM s ()
        checkCircles visited symbol = case Map.lookup symbol typedefs of
            Nothing -> return ()
            Just anno -> do
                assert (not $ Set.member symbol visited) "Typedef has circles"
                checkTypeCircles (Set.insert symbol visited) anno
        
        checkTypeCircles :: Set.Set Symbol -> Type -> DoM s ()
        checkTypeCircles visited typ = case typ of
            Void            -> return ()
--            Key t           -> return ()
            t | isSimple t  -> return ()
--            TypeApply symbol ts -> do
--                checkCircles visited symbol
--                mapM_ (checkTypeCircles visited) ts
--            Tuple ts        -> mapM_ (checkTypeCircles visited) ts
--            Table ts        -> mapM_ (checkTypeCircles visited) ts
--            Array n t       -> checkTypeCircles visited t
--            ADT fs          -> do
--                forM_ fs $ \field -> case field of
--                    FieldNull    -> return ()
--                    FieldType t  -> checkTypeCircles visited t
--                    FieldCtor ts -> mapM_ (checkTypeCircles visited) ts
            _                 -> fail ("checkTypeCircles " ++ show typ)
