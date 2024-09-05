{-# LANGUAGE FlexibleInstances #-}
module InferTypes where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import Type hiding (typeOfExpr)
import Monad
import InstBuilder
import ASTResolved
import Constraint
import Symbol
import AST
import Error


annotate :: InstBuilderState -> DoM Int InstBuilderState
annotate state = do
    types' <- forM (types state) (mapTypeM f)
    return state { types = types' }
    where
        f :: Type -> DoM Int Type
        f typ = case typ of
            Type 0 -> do
                n <- get
                modify (+1) 
                return (Type $ n + 1)
            _ -> return typ


deAnnotate :: InstBuilderState -> InstBuilderState
deAnnotate state = state { types = Map.map (mapType f) (types state) }
    where
        f :: Type -> Type
        f typ = case typ of
            Type n -> Type 0
            _ -> typ


data CollectState = CollectState
    { collected :: [Constraint]
    , astResolved :: ASTResolved
    , symbolTable :: Map.Map Symbol Type
    }

initCollectState ast = CollectState
    { collected = []
    , astResolved = ast
    , symbolTable = Map.empty
    }

instance TypeDefs (DoM CollectState) where
    getTypeDefs = gets (typeDefsAll . astResolved)


define :: Symbol -> Type -> DoM CollectState ()
define symbol typ = do
    resm <- gets $ Map.lookup symbol . symbolTable
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symbolTable = Map.insert symbol typ (symbolTable s) }

look :: Symbol -> DoM CollectState Type
look symbol = do
    resm <- gets $ Map.lookup symbol . symbolTable
    unless (isJust resm) (fail $ "undefined symbol: " ++ prettySymbol symbol)
    return (fromJust resm)


typeOfExpr :: InstBuilderState -> Expr -> Type
typeOfExpr state (Expr id) = (types state) Map.! id

typeOfPat :: InstBuilderState -> Pattern -> Type
typeOfPat state (Pattern id) = (types state) Map.! id

typeOfType :: InstBuilderState -> Type -> Type
typeOfType state (Type id) = (types state) Map.! id


constraint :: Type -> Type -> DoM CollectState ()
constraint t1 t2 = do
    modify $ \s -> s { collected = (ConsEq t1 t2) : (collected s) }

constraintDefault :: Type -> Type -> DoM CollectState ()
constraintDefault t1 t2 = do
    modify $ \s -> s { collected = (ConsDefault t1 t2) : (collected s) }


collectCall :: Type -> [Type] -> Type -> DoM CollectState ()
collectCall exprType exprTypes callType = do
    let (TypeDef funcSymbol, callTypeArgs) = unfoldType callType
    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
    unless (length exprTypes == length argTypes)
        (fail $ "invalid function type arguments: " ++ show callType)

    Just instances <- gets $ Map.lookup funcSymbol . instancesAll . astResolved
    features <- gets $ featuresAll . astResolved

    -- These indices describe the type variables which are independent according to the 
    -- functional dependencies. If the independent variables fully describe the call type,
    -- it can be said that no other overlapping definition could conflict with it so we can
    -- type check.
    let Feature _ featureGenerics funDeps _ _ _ = features Map.! funcSymbol
    unless (length callTypeArgs == length featureGenerics) (error "xs needs to be > 0")
    indices <- fmap catMaybes $ forM (zip featureGenerics [0..]) $ \(g, i) -> do
        case findIndex (g ==) (map snd funDeps) of
            Just _  -> return Nothing
            Nothing -> return (Just i) 

    fullAcqs <- fmap catMaybes $ forM (Map.elems $ instances) $ \stmt -> do
        appliedAcqType <- case stmt of
            Instance _ generics acqType _ _ _ -> do
                let genericsToVars = zip (map TypeDef generics) (map Type [-1, -2..])
                return (applyType genericsToVars acqType)

            Derives pos generics argType [featureType] -> do
                let acqType = Apply featureType argType
                let genericsToVars = zip (map TypeDef generics) (map Type [-1, -2..])
                return (applyType genericsToVars acqType)

        case typesCouldMatch appliedAcqType callType of
            False -> return Nothing
            True -> do
                let (TypeDef _, acqTypeArgs) = unfoldType appliedAcqType
                let b = all id $ map (\i -> typeFullyDescribes (acqTypeArgs !! i) (callTypeArgs !! i)) indices
                case b of
                    True -> return (Just appliedAcqType) 
                    False -> return Nothing

    case fullAcqs of
        [] -> return ()
        [acq] -> do
            subs <- unify =<< getConstraintsFromTypes acq callType
            (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf (applyType subs acq)
            constraint callType (applyType subs acq)
            constraint exprType retType
            zipWithM_ constraint argTypes exprTypes
            return ()
        x -> error (show x)
            
    constraint exprType retType
    zipWithM_ constraint argTypes exprTypes


collectDefault :: [Param] -> InstBuilderState -> DoM CollectState ()
collectDefault params state = do
    forM_ (Map.toList $ expressions state) $ \(id, expression) -> do
        let Just exprType = Map.lookup id (types state)
        case expression of
            Call _ callType exprs -> case unfoldType (typeOfType state callType) of
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["convert", "convert"]) -> do
                    constraintDefault exprType (typeOfExpr state $ exprs !! 0)

                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["tuple", "make2"]) -> do
                    constraintDefault exprType $ foldType (Tuple : map (typeOfExpr state) exprs)
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["tuple", "make3"]) -> do
                    constraintDefault exprType $ foldType (Tuple : map (typeOfExpr state) exprs)
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["tuple", "make4"]) -> do
                    constraintDefault exprType $ foldType (Tuple : map (typeOfExpr state) exprs)
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["tuple", "make5"]) -> do
                    constraintDefault exprType $ foldType (Tuple : map (typeOfExpr state) exprs)
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["tuple", "make6"]) -> do
                    constraintDefault exprType $ foldType (Tuple : map (typeOfExpr state) exprs)

                _ -> return ()
            
            _ -> return ()
            


collect :: Type -> [Param] -> InstBuilderState -> DoM CollectState ()
collect retty params state = do
    forM_ params $ \param -> case param of
        RefParam _ symbol typ -> define symbol typ
        Param _ symbol typ -> define symbol typ
        x -> error (show x)

    forM_ (Map.toList $ patterns state) $ \(id, pattern) -> withPos pattern $
        let Just patType = Map.lookup id (types state) in case pattern of
            PatIdent _ symbol -> define symbol patType
            x -> error (show x)
    forM_ (Map.toList $ statements state) $ \(id, statement) -> case statement of
        Assign _ symbol expr -> define symbol (typeOfExpr state expr)
        _ -> return ()

    forM_ (Map.toList $ expressions state) $ \(id, expression) -> withPos expression $ do
        let exprType = typeOfExpr state (Expr id)
        case expression of
            Ident _ symbol -> constraint exprType =<< look symbol
            Reference _ expr -> constraint exprType (typeOfExpr state expr)
            AST.Int _ _ -> constraint exprType I64
            AST.Float _ _ -> constraint exprType F64
            AST.Bool _ _ -> constraint exprType Type.Bool
            AST.Char _ _ -> constraint exprType Type.Char
            AST.String _ _ -> constraint exprType (Apply Slice Type.Char)

            Call _ (Type tid) exprs -> do
                let Just callType = Map.lookup tid (types state)
                let exprTypes = map (typeOfExpr state) exprs
                collectCall exprType exprTypes callType
        
            x -> error (show x)

    forM_ (Map.toList $ patterns state) $ \(id, pattern) -> withPos pattern $
        let Just patType = Map.lookup id (types state) in case pattern of
            PatIdent _ symbol -> constraint patType =<< look symbol
            x -> error (show x)
    
    forM_ (Map.toList $ statements state) $ \(id, statement) -> case statement of
        Block _ -> return ()
        ExprStmt _ -> return ()
        EmbedC _ _ _ -> return ()
        Let _ pat Nothing Nothing -> return ()
        Return _ Nothing -> return ()
        Return _ (Just expr) -> constraint (typeOfExpr state expr) retty
        Let _ pat (Just expr) Nothing -> constraint (typeOfPat state pat) (typeOfExpr state expr)
        Assign _ symbol expr -> constraint (typeOfExpr state expr) =<< look symbol
        If _ expr _ _ -> constraint (typeOfExpr state expr) Type.Bool
        While _ expr _ -> constraint (typeOfExpr state expr) Type.Bool
        x -> error (show x)


unifyOne :: MonadFail m => Constraint -> m [(Type, Type)]
unifyOne constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]

        (Apply a1 a2, Apply b1 b2) -> do
            subs1 <- unifyOne (ConsEq a1 b1)
            subs2 <- unifyOne (ConsEq a2 b2)
            return (subs1 ++ subs2)
            
        _ -> fail $ "type mismatch: " ++ show (t1, t2)

    ConsDefault t1 t2 -> case (t1, t2) of
        _ | t1 == t2 -> return []
        (Type _, _)  -> return [(t1, t2)]
        (_, _)       -> return []

    x -> error "invalid constraint"


unify :: MonadFail m => [Constraint] -> m [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (applyConstraint subs x)
    return (s ++ subs)


applyConstraint :: [(Type, Type)] -> Constraint -> Constraint
applyConstraint subs constraint = case constraint of
    ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
    ConsDefault t1 t2      -> ConsDefault (rf t1) (rf t2)
    where
        rf = applyType subs


inferTypes :: [Param] -> Type -> InstBuilderState -> DoM ASTResolved InstBuilderState
inferTypes params retty state = do
    (instAnnotated, _) <- runDoMExcept 0 (annotate state)
    deAnnotate <$> inferTypes' instAnnotated
    where
        inferTypes' :: InstBuilderState -> DoM ASTResolved InstBuilderState
        inferTypes' inst = do
            ast <- get
            (_, collectState)  <- runDoMExcept
                (initCollectState ast)
                (collect retty params inst)

            subs <- unify (collected collectState)
            let types' = Map.map (applyType subs) (types inst)

            if types' == types inst then return inst
            else inferTypes' $ inst { types = types' }


inferDefaults :: [Param] -> Type -> InstBuilderState -> DoM ASTResolved InstBuilderState
inferDefaults params retty state = do
    (instAnnotated, _) <- runDoMExcept 0 (annotate state)
    deAnnotate <$> inferTypes' instAnnotated
    where
        inferTypes' :: InstBuilderState -> DoM ASTResolved InstBuilderState
        inferTypes' inst = do
            ast <- get
            (_, collectState)  <- runDoMExcept
                (initCollectState ast)
                (collectDefault params inst)

            subs <- unify (collected collectState)
            let types' = Map.map (applyType subs) (types inst)

            if types' == types inst then return inst
            else inferTypes' $ inst { types = types' }



infer :: [Param] -> Type -> InstBuilderState -> DoM ASTResolved InstBuilderState
infer params retty state = do
    state' <- inferDefaults params retty =<< inferTypes params retty state
    if types state' == types state then return state'
    else infer params retty state'


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
