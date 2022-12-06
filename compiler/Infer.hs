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
infer :: BoM s m => ResolvedAst -> Map.Map String Collect.SymTab -> Bool -> m (ResolvedAst, Collect.SymTab)
infer ast imports verbose = do
    (inferedAst, symTab, n) <- recursiveInfer ast 
    return (inferedAst, symTab)
    where
        recursiveInfer :: BoM s m => ResolvedAst -> m (ResolvedAst, Collect.SymTab, Int)
        recursiveInfer ast = do
            -- run collect to get collect state containing type constraints
            (_, state) <- withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState imports $ moduleName ast) (collectAST ast)
            
            -- turn type constraints into substitutions using unify
            let symTabs = (symTab state) : Map.elems imports
            let sos     = concat $ map (SymTab.lookupKey Collect.KeyType) symTabs
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            let constraints = Map.toList (collected state)
            (subs, _) <- runBoMTExcept typeMap (unify2 constraints)

            -- if the infered ast is the same as the last iteration, finish
            let subbedAst = apply subs ast
            if ast == subbedAst
            then do
                (defaults, _) <- runBoMTExcept typeMap $ unifyDefault $
                    Map.toList $ Map.mapKeys (apply subs) (defaults state)
                let defaultedAst = apply defaults subbedAst
                let defaultedSymTab = apply defaults $ apply subs $ symTab state

                if defaultedAst == subbedAst then do
                    return (defaultedAst, defaultedSymTab, 1)
                else do
                    (subbedAst', symTab, n) <- recursiveInfer defaultedAst 
                    return (subbedAst', symTab, n + 1)
            else do
                (subbedAst', symTab, n) <- recursiveInfer subbedAst
                return (subbedAst', symTab, n + 1)


