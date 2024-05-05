module CombineAsts where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import ASTResolved
import Monad
import Symbol


initAstResolved = ASTResolved
    { moduleName = ""
    , includes   = Set.empty
    , links      = Set.empty
    , topTypedefs = []
    , topFuncdefs = []
    , topFeatures = []
    }


combineAsts :: AST -> [ASTResolved] -> DoM s ASTResolved
combineAsts ast imports = fmap snd (runDoMExcept initAstResolved combineAsts')
    where
        combineAsts' :: DoM ASTResolved ()
        combineAsts' = do
            let includes' = [ x | (CInclude x) <- astImports ast ]
            let links'    = [ x | (CLink x)    <- astImports ast ]

            modify $ \s -> s
                { moduleName  = astModuleName ast
                , includes    = Set.union (includes s) (Set.fromList includes')
                , links       = Set.union (links s)    (Set.fromList links')
                , topTypedefs = [ x | x@(Typedef _ _ _ _) <- astStmts ast ]
                , topFuncdefs = [ x | x@(FuncDef _)       <- astStmts ast ]
                , topFeatures = [ x | x@(Feature _ _ _)   <- astStmts ast ]
                }

            return ()

