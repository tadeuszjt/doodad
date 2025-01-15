module FindFunc where

import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

import Monad
import AST
import ASTResolved
import Type
import Error
import IR
import InstBuilder
import AstBuilder


data Constraint
    = ConsEq Type Type
    deriving (Eq, Ord, Show)


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

applyConstraint :: [(Type, Type)] -> Constraint -> Constraint
applyConstraint subs constraint = case constraint of
    ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
    where
        rf = applyType subs



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


-- this function converts to funcIrHeader (when it shouldn't)
makeHeaderInstantiation :: Type -> DoM ASTResolved (Maybe FuncIrHeader)
makeHeaderInstantiation callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    let (TypeDef callSymbol, _) = unfoldType callType
    Just instances <- gets $ Map.lookup callSymbol . instancesAll
    results <- fmap catMaybes $ forM (Map.toList instances) $ \(symbol, stmt) -> case stmt of
        TopStmt (Derives _ generics argType [featureType]) -> do
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
                    makeHeaderInstantiation lower

        TopField pos generics acqType i -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs acqType

            subsEither <- tryError (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType) (fail $ "type mismatch: " ++ show callType)
                    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
                    args' <- forM argTypes $ \t -> return (ParamIR Value t)
                    retty' <- return (RettyIR IR.Value retType)
                    return $ Just $ FuncIrHeader
                        { irFuncSymbol = symbol
                        , irArgs = args'
                        , irRetty = retty'
                        }


        TopInst pos generics implType args retty inst -> do
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

                    args' <- forM (zip args argTypes) $ \(arg, t) -> case (arg, t) of
                        (RefParam pos _ _, Apply Type.Slice typ)-> return (ParamIR IR.Slice typ)
                        (Param _ _ _, Apply Type.Slice typ) -> return (ParamIR IR.Slice typ)
                        (RefParam _ _ _, _)                 -> return (ParamIR IR.Ref t)
                        (Param _ _ _, _)                    -> return (ParamIR Value t)
                    retty' <- case (retty, retType) of
                        (Retty _,    Apply Type.Slice t) -> return (RettyIR IR.Slice t)
                        (RefRetty _, Apply Type.Slice t) -> return (RettyIR IR.Slice t)
                        (Retty _, _)                     -> return (RettyIR IR.Value retType)
                        (RefRetty _, _)                  -> return (RettyIR IR.Ref retType)

                    return $ Just $ FuncIrHeader
                        { irFuncSymbol = symbol
                        , irArgs = args'
                        , irRetty = retty'
                        }

        _ -> return Nothing

    case results of
        []     -> return Nothing
        [func] -> return (Just func)
        funcs  -> fail $ "multiple instances for: " ++ show callType


makeInstantiation :: Type -> DoM ASTResolved (Maybe TopStmt)
makeInstantiation callType = do
    -- In haskell, instances are globally visible, so we do not have to worry about different instances.
    let (TypeDef callSymbol, _) = unfoldType callType
    Just instances <- gets $ Map.lookup callSymbol . instancesAll
    results <- fmap catMaybes $ forM (Map.toList instances) $ \(symbol, stmt) -> case stmt of
        TopStmt (Derives _ generics argType [featureType]) -> do
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
                    makeInstantiation lower

        TopField pos generics acqType i -> do
            let genericSubs = zip (map TypeDef generics) (map Type [1..])
            let typ = applyType genericSubs acqType
            subsEither <- tryError (unify =<< getConstraintsFromTypes typ callType)
            case subsEither of
                Left _ -> return Nothing
                Right subs -> do
                    unless (applyType subs typ == callType) (fail $ "type mismatch: " ++ show callType)
                    return $ Just $ TopField pos [] callType i

        TopInst pos generics implType args retty inst -> do
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
                    let inst' = inst { types = Map.map (applyType subs . applyType genericSubs) (types inst) }
                    let args'   = zipWith (\t p -> p { paramType = t}) argTypes args 
                    retty' <- case retty of
                        Retty _ -> return (Retty retType)
                        RefRetty _ -> return (RefRetty retType)
                    return $ Just $ TopInst pos [] callType args' retty' inst'

    case results of
        []     -> do
            fail $ "no acquire for: " ++ show callType
            return Nothing
        [func] -> do
            return (Just func)
        funcs  -> fail $ "multiple instances for: " ++ show callType

