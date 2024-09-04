module FindFunc where

import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

import Monad
import AST
import ASTResolved
import Type
import Apply
import Constraint
import Error
import Symbol
import IR


unifyOne :: MonadFail m => Constraint -> m [(Type, Type)]
unifyOne constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2  -> return []
        (Type _, _)   -> return [(t1, t2)]
        (_, Type _)   -> return [(t2, t1)]

        (Apply a1 b1, Apply a2 b2) -> do    
            subs1 <- unifyOne (ConsEq a1 a2)
            subs2 <- unifyOne (ConsEq b1 b2)
            return (subs1 ++ subs2)

        _ -> fail $ "cannot unify: " ++ show (t1, t2)


unify :: MonadFail m => [Constraint] -> m [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (applyConstraint subs x)
    return (s ++ subs)


getConstraintsFromTypes :: MonadFail m => Type -> Type -> m [Constraint]
getConstraintsFromTypes t1 t2 = case (t1, t2) of
    (a, b) | a == b  -> return [] 
    (Type _, _)      -> return [ConsEq t1 t2]
    (_, Type _)      -> return [ConsEq t2 t1]

    (Apply a1 a2, Apply b1 b2) -> do
        subs1 <- getConstraintsFromTypes a1 b1
        subs2 <- getConstraintsFromTypes a2 b2
        return (subs1 ++ subs2)

    _ -> fail $ show (t1, t2)


makeHeaderInstance :: Type -> DoM ASTResolved (Maybe FuncIrHeader)
makeHeaderInstance callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    let (TypeDef callSymbol, _) = unfoldType callType
    Just instances <- gets $ Map.lookup callSymbol . instancesAll
    results <- fmap catMaybes $ forM (Map.toList instances) $ \(symbol, stmt) -> case stmt of
        Derives _ generics argType [featureType] -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let upperType = applyType genericSubs (Apply featureType argType)

            subsEither <- tryError (unify =<< getConstraintsFromTypes upperType callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    lowerArgType <- fromJust <$> lowerTypeOfm argType
                    let lowerType = applyType genericSubs (Apply featureType lowerArgType)
                    let lower = applyType subs lowerType
                    unless (applyType subs upperType == callType) (fail $ "type mismatch: " ++ show callType)
                    unless (typeFullyResolved lower) (error "propagating type vars")
                    makeHeaderInstance lower

        Instance pos generics implType args isRef scope -> do
            --liftIO $ putStrLn $ "checking aquire: " ++ prettySymbol symbol
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs implType

            subsEither <- tryError (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType) (fail $ "type mismatch: " ++ show callType)
                    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
                    unless (length argTypes == length args) (error "something else went wrong")

                    args' <- forM (zip args argTypes) $ \(arg, t) -> case arg of
                        RefParam _ _ _ -> return (ParamIR Ref t)
                        Param _ _ _    -> return (ParamIR Value t)
                    return $ Just $ FuncIrHeader
                        { irFuncSymbol = symbol
                        , irArgs       = args'
                        , irRetty      = RettyIR (if isRef then Ref else Value) retType
                        }

        _ -> return Nothing

    case results of
        []     -> return Nothing
        [func] -> return (Just func)
        funcs  -> fail $ "multiple instances for: " ++ show callType


makeInstance :: Type -> DoM ASTResolved (Maybe AST.Stmt)
makeInstance callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    let (TypeDef callSymbol, _) = unfoldType callType
    Just instances <- gets $ Map.lookup callSymbol . instancesAll
    results <- fmap catMaybes $ forM (Map.toList instances) $ \(symbol, stmt) -> case stmt of
        Derives _ generics argType [featureType] -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let upperType = applyType genericSubs $ Apply featureType argType

            subsEither <- tryError (unify =<< getConstraintsFromTypes upperType callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    lowerArgType <- fromJust <$> lowerTypeOfm argType
                    let lowerType = applyType genericSubs (Apply featureType lowerArgType)
                    let lower = applyType subs lowerType

                    unless (applyType subs upperType == callType) (fail $ "type mismatch: " ++ show callType)
                    unless (typeFullyResolved lower) (error "propagating type vars")
                    makeInstance lower

        Instance pos generics implType args isRef scope -> do
            --liftIO $ putStrLn $ "checking aquire: " ++ prettySymbol symbol
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs implType

            subsEither <- tryError (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType) (fail $ "type mismatch: " ++ show callType)
                    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
                    unless (length argTypes == length args) (error "something else went wrong")

                    -- args need to be swapped from void
                    let args'   = zipWith (\t p -> p { paramType = t}) argTypes args 
                    let retty'  = (if isRef then RefRetty else Retty) retType
                    let stmt'   = applyStmt subs (applyStmt genericSubs scope)
                    return $ Just $ AST.FuncInst pos [] symbol args' retty' stmt'

    case results of
        []     -> do
            fail $ "no acquire for: " ++ show callType
            return Nothing
        [func] -> do
            return (Just func)
        funcs  -> fail $ "multiple instances for: " ++ show callType

