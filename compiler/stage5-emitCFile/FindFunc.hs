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

        (Apply _ _, Apply _ _) -> error "here"

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

    (Apply t1 ts1, Apply t2 ts2) | length ts1 == length ts2 ->
        concat <$> zipWithM (getConstraintsFromTypes) (t1 : ts1) (t2 : ts2)

    _ -> fail $ show (t1, t2)



findFunction :: Type -> DoM ASTResolved Func
findFunction funcType = do
    (symbol, typeArgs) <- case funcType of
        TypeDef symbol                  -> return (symbol, [])
        Apply (TypeDef symbol) typeArgs -> return (symbol, typeArgs)

    isFunc <- gets (Map.member symbol . funcDefsAll)
    case isFunc of
        True -> do
            Just func <- gets (Map.lookup symbol . funcDefsAll)
            Just (generics, _) <- gets (Map.lookup symbol . typeDefsAll)
            unless (length generics == length typeArgs) (error "type mismatch")
            let subs = zip (map TypeDef generics) typeArgs
            return (applyFunc subs func)

        False -> do -- is aquire
            acquiresAll <- gets acquiresAll

            results <- fmap catMaybes $ forM (Map.toList acquiresAll) $ \(symbol, stmt) -> do
                --liftIO $ putStrLn $ "checking aquire: " ++ prettySymbol symbol
                let Aquires pos generics typ args isRef scope = stmt
                let genericSubs = zipWith (\g i -> (TypeDef g, Type i)) generics [1..]
                let appliedType = applyType genericSubs typ

                subsEither <- tryError (unify =<< getConstraintsFromTypes appliedType funcType)

                case subsEither of
                    Right subs -> do
                        unless (applyType subs appliedType == funcType)
                            (error "something went terribly wrong")

                        Apply Type.Func (retType : argTypes) <- baseTypeOf funcType
                        unless (length argTypes == length args) (error "something else went wrong")

                        -- args need to be swapped from void
                        let args'   = zipWith (\t p -> p { paramType = t}) argTypes args 
                        let retty'  = (if isRef then RefRetty else Retty) retType
                        let stmt'   = applyStmt subs (applyStmt genericSubs scope)
                        return $ Just $ AST.Func (FuncHeader pos symbol args' retty') stmt'

                    Left e -> do
                        --liftIO $ putStrLn $ "\tLeft: " ++ show e
                        return Nothing

            case results of
                [] -> fail $ "no valid acquires for: " ++ show funcType
                [func] -> return func

