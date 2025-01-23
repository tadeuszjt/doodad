module IrContextPass where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Type
import Symbol
import Ir2
import Monad
import ASTResolved



irContextPass :: DoM ASTResolved ()
irContextPass = do
    mapM irContextAdd . Set.toList =<< gets instantiationsTop

    return ()



irContextAdd :: Type -> DoM ASTResolved ()
irContextAdd instType = do
    modify $ \s -> s { instantiations = Map.insert
        instType
        ((instantiations s Map.! instType) { irContexts = Just (Set.empty) })
        (instantiations s) }

    Just funcIr <- gets (Map.lookup instType . instantiations)

    contexts <- fmap (Set.fromList . concat) $ forM (irStmts funcIr) $ \stmt -> do
        case stmt of
            Call typ args -> case isBuiltinContext typ of
                Just t -> return [t]
                Nothing -> do
                    callContexts <- gets $ irContexts . (Map.! typ) . instantiations
                    case callContexts of
                        Nothing -> do
                            irContextAdd typ
                            Just callIr' <- gets (Map.lookup typ . instantiations)
                            return $ maybe [] Set.toList (irContexts callIr')

                        Just xs -> return (Set.toList xs)

            _ -> return []

    modify $ \s -> s { instantiations = Map.insert
        instType
        ((instantiations s Map.! instType) { irContexts = Just contexts })
        (instantiations s) }

    --liftIO $ putStrLn $ show instType ++ " " ++ show contexts

    return ()
            

isBuiltinContext :: Type -> Maybe Type
isBuiltinContext typ = let (TypeDef symbol, ts) = unfoldType typ in
    case symbolsCouldMatch symbol (Sym ["builtin", "builtinContext"]) of
        False -> Nothing
        True -> case ts of
            [t] -> Just t


