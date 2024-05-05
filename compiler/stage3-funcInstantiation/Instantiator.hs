module Instantiator where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.IO.Class

import Monad
import ASTResolved
import ASTMapper
import AST
import Type
import Symbol


instantiate :: ASTResolved -> DoM s ASTResolved
instantiate ast = fmap snd (runDoMExcept ast instantiate')
    where
        instantiate' :: DoM ASTResolved ()
        instantiate' = do
            ast <- get

            -- create initial instantiations
            forM_ (topFuncdefs ast) $ \(FuncDef (Func header stmt)) -> do
                when (funcGenerics header == []) $ do
                    let callHeader = CallHeader
                            (funcSymbol header)
                            (map typeof $ funcArgs header)
                            (typeof $ funcRetty header)

                    resm <- gets $ Map.lookup callHeader . funcInstances
                    unless (isNothing resm) (error "function instance already created")

                    modify $ \s -> s { funcInstances = Map.insert callHeader (Func header stmt) (funcInstances s) }



            


instantiatorMapper :: Elem -> DoM ASTResolved Elem
instantiatorMapper element = case element of
    ElemStmt (FuncDef (Func header stmt)) -> do
        let isGenericFunc = any isGeneric $ (typeof $ funcRetty header) : (map typeof $ funcArgs header)

        case isGenericFunc of
            True -> liftIO $ putStrLn $ "generic: " ++ show (funcSymbol header)
            False -> liftIO $ putStrLn $ "not generic: " ++ show (funcSymbol header)
        return element

    _ -> return element


isGeneric :: Type -> Bool
isGeneric typ = case typ of
    TypeApply (SymResolved str) _
        | "type" `elem` (parseStr str) -> True
    _ -> False
    
