module FindFunc where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import AST
import ASTResolved
import Type
import Error
import InstBuilder
import AstBuilder


data Constraint
    = ConsEq Type Type
    deriving (Eq, Ord, Show)


unifyOne :: Constraint -> Except Error [(Type, Type)]
unifyOne constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2  -> return []
        (Type _, _)   -> return [(t1, t2)]
        (_, Type _)   -> return [(t2, t1)]

        (Apply a1 b1, Apply a2 b2) -> do    
            subs1 <- unifyOne (ConsEq a1 a2)
            subs2 <- unifyOne (ConsEq b1 b2)
            return (subs1 ++ subs2)

        _ -> throwError $ ErrorStr $ "cannot unify: " ++ show (t1, t2)


unify :: [Constraint] -> Except Error [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (applyConstraint subs x)
    return (s ++ subs)


applyConstraint :: [(Type, Type)] -> Constraint -> Constraint
applyConstraint subs constraint = case constraint of
    ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
    where
        rf = applyType subs


getConstraintsFromTypes :: Type -> Type -> Except Error [Constraint]
getConstraintsFromTypes t1 t2 = case (t1, t2) of
    (a, b) | a == b  -> return [] 
    (Type _, _)      -> return [ConsEq t1 t2]
    (_, Type _)      -> return [ConsEq t2 t1]

    (Apply a1 a2, Apply b1 b2) -> do
        subs1 <- getConstraintsFromTypes a1 b1
        subs2 <- getConstraintsFromTypes a2 b2
        return (subs1 ++ subs2)

    _ -> throwError $ ErrorStr $ show (t1, t2)


makeInstantiation :: ASTResolved -> Type -> Except Error (Maybe TopStmt)
makeInstantiation ast callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    results <- fmap catMaybes $ forM (Map.toList $ instancesAll ast) $ \(symbol, stmt) -> case stmt of
        TopStmt (Derives _ generics argType [featureType]) -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let upperType = applyType genericSubs $ Apply featureType argType

            let subsEither = runExcept (unify =<< getConstraintsFromTypes upperType callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    let Right (Just lowerArgType) = runTypeDefsMonad (typeDefsAll ast) 
                            (lowerTypeOfm argType)
                    let lowerType = applyType genericSubs (Apply featureType lowerArgType)
                    let lower = applyType subs lowerType

                    unless (applyType subs upperType == callType)
                        (throwError $ ErrorStr $ "type mismatch: " ++ show callType)
                    unless (typeFullyResolved lower) (error "propagating type vars")
                    makeInstantiation ast lower

        TopField pos generics acqType i -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs acqType
            let subsEither = runExcept (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType)
                        (throwError $ ErrorStr $ "type mismatch: " ++ show callType)
                    return $ Just $ TopField pos [] callType i

        TopInst pos generics implType args isRef inst -> do
            --liftIO $ putStrLn $ "checking aquire: " ++ prettySymbol symbol
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs implType

            let subsEither = runExcept (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType)
                        (throwError $ ErrorStr $ "type mismatch: " ++ show callType)
                    let Right base = runTypeDefsMonad (typeDefsAll ast) (baseTypeOf callType)
                    let (Type.Func, retType : argTypes) = unfoldType base
                        
                    unless (length argTypes == length args) (error "something else went wrong")

                    -- args need to be swapped from void
                    let inst' = inst { types = Map.map (applyType subs . applyType genericSubs) (types inst) }
                    let args'   = zipWith (\t p -> p { paramType = t}) argTypes args 
                    return $ Just $ TopInst pos [] callType args' isRef inst'

    case results of
        []     -> do
            throwError $ ErrorStr $ "no acquire for: " ++ show callType
            return Nothing
        [func] -> do
            return (Just func)
        funcs  -> throwError $ ErrorStr $ "multiple instances for: " ++ show callType

