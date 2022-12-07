module Infer where

import System.FilePath
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import qualified SymTab
import AST
import Args
import Monad
import Error
import Interop
import Apply
import Unify
import Collect
import qualified Resolve
import States


-- Takes a resolved and annotated ast and inferes all types.
infer :: BoM s m => ResolvedAst -> Bool -> m ResolvedAst
infer ast verbose = fst <$> recursiveInfer ast
    where
        recursiveInfer :: BoM s m => ResolvedAst -> m (ResolvedAst, Int)
        recursiveInfer ast = do
            -- run collect to get collect state containing type constraints
            (_, state) <- withErrorPrefix "collect: " $
                runBoMTExcept initCollectState (collectAST ast)
            
            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab state)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            (subs, _) <- runBoMTExcept typeMap (unify2 $ Map.toList $ collected state)

            -- if the infered ast is the same as the last iteration, finish
            let subbedAst = apply subs ast
            if ast == subbedAst
            then do
                (defaults, _) <- runBoMTExcept typeMap $ unifyDefault $
                    Map.toList $ Map.mapKeys (apply subs) (defaults state)
                let defaultedAst = apply defaults subbedAst

                if defaultedAst == subbedAst then do
                    return (defaultedAst, 1)
                else do
                    (subbedAst', n) <- recursiveInfer defaultedAst 
                    return (subbedAst', n + 1)
            else do
                (subbedAst', n) <- recursiveInfer subbedAst
                return (subbedAst', n + 1)
