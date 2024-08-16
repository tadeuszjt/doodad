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



makeFunctionInstance :: Type -> DoM ASTResolved Func
makeFunctionInstance funcType = do
    let (TypeDef symbol, typeArgs) = unfoldType funcType

    Just func          <- gets (Map.lookup symbol . funcDefsAll)
    Just (generics, _) <- gets (Map.lookup symbol . typeDefsAll)

    unless (length generics == length typeArgs) (error $ "type mismatch for: " ++ show symbol)

    let subs = zip (map TypeDef generics) typeArgs
    return (applyFunc subs func)



makeAcquireInstance :: Type -> DoM ASTResolved Func
makeAcquireInstance callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    acquiresAll <- gets acquiresAll
    featuresAll <- gets featuresAll
    typeDefsAll <- gets typeDefsAll
    results <- fmap catMaybes $ forM (Map.toList acquiresAll) $ \(symbol, stmt) -> case stmt of
        Derives _ generics argType featureType -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let upperType = applyType genericSubs $ Apply featureType argType

            subsEither <- tryError (unify =<< getConstraintsFromTypes upperType callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    lowerArgType <- fromJust <$> lowerTypeOfm argType
                    let lowerType = applyType genericSubs (Apply featureType lowerArgType)
                    let lower = applyType subs lowerType

                    unless (applyType subs upperType == callType) (error "type mismatch")
                    unless (typeFullyResolved lower) (error "propagating type vars")
                    Just <$> makeAcquireInstance lower

        Aquires pos generics implType args isRef scope -> do
            --liftIO $ putStrLn $ "checking aquire: " ++ prettySymbol symbol
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs implType

            subsEither <- tryError (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType) (error $ "type mismatch: " ++ show callType)
                    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
                    unless (length argTypes == length args) (error "something else went wrong")

                    -- args need to be swapped from void
                    let args'   = zipWith (\t p -> p { paramType = t}) argTypes args 
                    let retty'  = (if isRef then RefRetty else Retty) retType
                    let stmt'   = applyStmt subs (applyStmt genericSubs scope)
                    return $ Just $ AST.Func (FuncHeader pos symbol args' retty') stmt'

    case results of
        [] -> fail $ "no valid acquires for: " ++ show callType
        [func] -> return func
        funcs -> fail $ "multiple acquires for: " ++ show callType

