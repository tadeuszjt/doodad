module Infer where

import System.FilePath
import Control.Monad.State

import qualified Data.Map as Map
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


-- Takes a resolved and annotated ast and inferes all types.
infer ::
    BoM s m =>
    AST ->
    [Extern] ->
    Map.Map String Collect.SymTab ->
    String ->
    Bool ->
    m (AST, Collect.SymTab)
infer ast externs imports modName verbose = do
    -- create C imports module
    assert (not $ Map.member "c" imports) "Module named c already imported"
    cSymTab <- fmap (symTab . snd) $
        runBoMTExcept (initCollectState Map.empty modName) (collectCExterns externs)

    -- run recursive algorithm
    (inferedAst, symTab, n) <- recursiveInfer ast (Map.insert "c" cSymTab imports)

    when verbose $ liftIO $
        putStrLn ("infered " ++ modName ++ " after " ++ show n ++ " iterations")

    return (inferedAst, symTab)
    where
        recursiveInfer :: BoM s m => AST -> Map.Map String Collect.SymTab -> m (AST, Collect.SymTab, Int)
        recursiveInfer ast imports = do
            -- run collect to get collect state containing type constraints
            (_, state) <- withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState imports modName) (collectAST ast)
            
            -- turn type constraints into substitutions using unify
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList $ SymTab.lookupKey Collect.KeyType (symTab state)
            (subs, _) <- runBoMTExcept typeMap (unify $ collected state)

            --liftIO $ putStrLn $ modName ++ " substitutions:"
            --liftIO $ mapM_ (putStrLn . show) subs

            -- if the infered ast is the same as the last iteration, finish
            let subbedAst = apply subs ast
            if ast == subbedAst
            then do
                (defaults, _) <- runBoMTExcept typeMap $ unifyDefault $ map (apply subs) (defaults state)
                --liftIO $ putStrLn $ modName ++ " defaults:"
                --liftIO $ mapM_ (putStrLn . show) defaults

                let defaultedAst = apply defaults subbedAst
                let defaultedSymTab = apply defaults $ apply subs $ symTab state

                if defaultedAst == ast then do
                    return (defaultedAst, defaultedSymTab, 1)
                else do
                    (subbedAst', symTab, n) <- recursiveInfer defaultedAst imports
                    return (subbedAst', symTab, n + 1)
            else do
                (subbedAst', symTab, n) <- recursiveInfer subbedAst imports
                return (subbedAst', symTab, n + 1)


