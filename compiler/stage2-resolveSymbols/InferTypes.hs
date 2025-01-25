{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module InferTypes where

import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import Type hiding (typeOfExpr)
import InstBuilder
import ASTResolved
import Symbol
import AST
import Error
import AstBuilder


data CollectState = CollectState
    { collected :: [Constraint]
    , symbolTable :: Map.Map Symbol Type
    }


initCollectState = CollectState
    { collected = []
    , symbolTable = Map.empty
    }


newtype Collect a = Collect
    { unCollect :: StateT CollectState (ReaderT ASTResolved (Except Error)) a }
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState CollectState,
        MonadError Error,
        MonadReader ASTResolved)


instance MonadFail Collect where
    fail s = throwError (ErrorStr s)


instance MonadTypeDefs Collect where
    getTypeDefs = typeDefsAll <$> ask


runCollect :: CollectState -> ASTResolved -> Collect a -> Except Error (a, CollectState)
runCollect collectState ast f =
    runReaderT (runStateT (unCollect f) collectState) ast


annotate :: InstBuilderState -> State Int InstBuilderState
annotate state = do
    types' <- forM (types state) (mapTypeM f)
    return state { types = types' }
    where
        f :: Type -> State Int Type
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


data Constraint
    = ConsEq Type Type
    | ConsDefault Type Type
    deriving (Eq, Ord, Show)


define :: Symbol -> Type -> Collect ()
define symbol typ = do
    resm <- gets (Map.lookup symbol . symbolTable)
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symbolTable = Map.insert symbol typ (symbolTable s) }


look :: Symbol -> Collect Type
look symbol = do
    resm <- gets $ Map.lookup symbol . symbolTable
    unless (isJust resm) (fail $ "undefined symbol: " ++ prettySymbol symbol)
    return (fromJust resm)


constraint :: Type -> Type -> Collect ()
constraint t1 t2 = do
    modify $ \s -> s { collected = (ConsEq t1 t2) : (collected s) }


constraintDefault :: Type -> Type -> Collect ()
constraintDefault t1 t2 = do
    modify $ \s -> s { collected = (ConsDefault t1 t2) : (collected s) }


collectCall :: Type -> [Type] -> Type -> Collect ()
collectCall exprType exprTypes callType = do
    let (TypeDef funcSymbol, callTypeArgs) = unfoldType callType
    (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
    unless (length exprTypes == length argTypes)
        (fail $ "invalid function type arguments: " ++ show callType)

    -- These indices describe the type variables which are independent according to the 
    -- functional dependencies. If the independent variables fully describe the call type,
    -- it can be said that no other overlapping definition could conflict with it so we can
    -- type check.
    Just (Function _ featureGenerics funDeps _ _) <- Map.lookup funcSymbol . featuresAll <$> ask
    unless (length callTypeArgs == length featureGenerics) (error "xs needs to be > 0")
    let indices = map snd $ filter
            (\(g, _) -> not $ elem g $ map snd funDeps)
            (zip featureGenerics [0..])

    instances <- instancesAll <$> ask

    let appliedInstTypes = filter (typesCouldMatch callType) $
            (flip map) (Map.elems instances) $ \stmt -> do
            let (generics, typ) = case stmt of
                    TopInst _ generics typ _ _ _                       -> (generics, typ)
                    TopField _ generics typ _                          -> (generics, typ)
                    TopStmt (Derives _ generics argType [featureType]) ->
                        (generics, Apply featureType argType) in

                let genericsToVars = zip (map TypeDef generics) (map Type [-1, -2..]) in 
                    applyType genericsToVars typ


    let fullAcqs = (flip filter) appliedInstTypes $ \appliedInstType ->
            let (TypeDef _, acqTypeArgs) = unfoldType appliedInstType in
                all id $ map (\i -> typeFullyDescribes (acqTypeArgs !! i) (callTypeArgs !! i)) indices

    case fullAcqs of
        [] -> return ()
        [acq] -> do
            let Right subs = runExcept $ unify =<< getConstraintsFromTypes acq callType
            (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf (applyType subs acq)
            constraint callType (applyType subs acq)
            constraint exprType retType
            zipWithM_ constraint argTypes exprTypes
            return ()
        x -> error (show x)
            
    constraint exprType retType
    zipWithM_ constraint argTypes exprTypes


collectDefault :: [Param] -> InstBuilderState -> Collect ()
collectDefault params state = do
    forM_ (Map.toList $ expressions state) $ \(id, expression) -> do
        let Just exprType = Map.lookup id (types state)
        case expression of
            Call _ callType exprs -> case unfoldType (typeOfType state callType) of
                (TypeDef symbol, ts) | symbolsCouldMatch symbol (Sym ["convert", "convert"]) -> do
                    constraintDefault exprType (typeOfExpr state $ exprs !! 0)

                (TypeDef symbol, [t, s]) | symbolsCouldMatch symbol (Sym ["slice", "makeSlice"]) -> do
                    constraintDefault exprType (foldType [Slice, t])

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
            

getSymbol :: Symbol -> Collect Symbol
getSymbol symbol = do
    xs <- filter (symbolsCouldMatch symbol) . Map.keys . typeDefsAll <$> ask
    case xs of
        [x] -> return x
        _ -> error (prettySymbol symbol)


collect :: Type -> [Param] -> InstBuilderState -> Collect ()
collect retty params state = do
    forM_ params $ \param -> case param of
        RefParam _ symbol typ -> define symbol typ
        Param _ symbol typ -> define symbol typ
        x -> error (show x)

    forM_ (Map.toList $ patterns state) $ \(id, pattern) -> withPos pattern $
        let Just patType = Map.lookup id (types state) in case pattern of
            PatIdent _ symbol -> define symbol patType
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

            AST.Array _ exprs -> do
                when (length exprs > 0) $ do
                    constraint exprType (Apply Type.Slice $ typeOfExpr state $ exprs !! 0)
                    forM_ exprs $ \expr -> do
                        constraint (typeOfExpr state expr) (typeOfExpr state $ exprs !! 0)

            Call _ (Type tid) exprs -> do
                let Just callType = Map.lookup tid (types state)
                let exprTypes = map (typeOfExpr state) exprs
                collectCall exprType exprTypes callType

            Match _ expr pat -> do
                constraint exprType Type.Bool
                constraint (typeOfExpr state expr) (typeOfPat state pat)
        
            x -> error (show x)

    forM_ (Map.toList $ patterns state) $ \(id, pattern) -> withPos pattern $
        let Just patType = Map.lookup id (types state) in case pattern of
            PatIdent _ symbol -> constraint patType =<< look symbol
            PatLiteral expr -> constraint patType (typeOfExpr state expr)

            PatGuarded _ pat expr -> do
                constraint (typeOfExpr state expr) (Type.Bool)
                constraint patType (typeOfPat state pat)

            PatField _ symbol pat -> do
                Just (sumSym, i) <- (Map.lookup symbol . fieldsAll) <$> ask
                resm <- baseTypeOfm patType
                case resm of
                    Nothing -> return ()
                    Just base -> do
                        let (TypeDef sumSymbol, _) = unfoldType patType
                        unless (sumSymbol == sumSym) (fail "sum symbols don't match")
                        let (Sum, ts) = unfoldType base
                        constraint (typeOfPat state pat) (ts !! i)

            PatTuple _ pats -> do
                forM_ (zip pats [0..]) $ \(pat@(Pattern pid), i) -> do
                    let Just pType = Map.lookup pid (types state)
                    symbol <- getSymbol (Sym ["tuple", "tuplePattern"])
                    collectCall pType [patType] $ foldType [TypeDef symbol, pType, Size (length pats), Size i, patType]

            PatIgnore _ -> return ()

            x -> error (show x)
    
    forM_ (Map.toList $ statements state) $ \(id, statement) -> case statement of
        Block _ -> return ()
        ExprStmt _ -> return ()
        EmbedC _ _ _ -> return ()
        Let _ pat Nothing Nothing -> return ()
        With _ _ _ -> return ()
        Return _ Nothing -> return ()
        Return _ (Just expr) -> constraint (typeOfExpr state expr) retty
        Let _ pat (Just expr) Nothing -> constraint (typeOfPat state pat) (typeOfExpr state expr)
        If _ expr _ _ -> constraint (typeOfExpr state expr) Type.Bool
        While _ expr _ -> constraint (typeOfExpr state expr) Type.Bool
        Switch _ expr cases -> forM_ cases $ \(pat, _) -> constraint (typeOfPat state pat) (typeOfExpr state expr)
        For _ expr mpat _ -> case mpat of
            Nothing -> return ()
            Just pat -> do
                symbol <- getSymbol (Sym ["container", "forAt"])
                collectCall (typeOfPat state pat) [typeOfExpr state expr, I64] $
                    foldType [TypeDef symbol, typeOfPat state pat, typeOfExpr state expr]
                        
                

        x -> error (show x)


unifyOne :: Constraint -> Except Error [(Type, Type)]
unifyOne constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]

        (Apply a1 a2, Apply b1 b2) -> do
            subs1 <- unifyOne (ConsEq a1 b1)
            subs2 <- unifyOne (ConsEq a2 b2)
            return (subs1 ++ subs2)
            
        _ -> throwError $ ErrorStr $ "type mismatch: " ++ show (t1, t2)

    ConsDefault t1 t2 -> case (t1, t2) of
        _ | t1 == t2 -> return []
        (Type _, _)  -> return [(t1, t2)]
        (_, _)       -> return []

    x -> error "invalid constraint"


unify :: [Constraint] -> Except Error [(Type, Type)]
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


inferTypes :: ASTResolved -> [Param] -> Type -> InstBuilderState -> Except Error InstBuilderState
inferTypes ast params retty state = do
    fmap deAnnotate $ inferTypes' $ evalState (annotate state) 0
    where
        inferTypes' :: InstBuilderState -> Except Error InstBuilderState
        inferTypes' inst = do
            (_, collectState) <- runCollect initCollectState ast (collect retty params inst)
            subs <- unify (collected collectState)
            let types' = Map.map (applyType subs) (types inst)
            if types' == types inst then
                return inst
            else
                inferTypes' inst{ types = types' }


inferDefaults :: ASTResolved -> [Param] -> Type -> InstBuilderState -> Except Error InstBuilderState
inferDefaults ast params retty state = do
    fmap deAnnotate $ inferTypes' $ evalState (annotate state) 0
    where
        inferTypes' :: InstBuilderState -> Except Error InstBuilderState
        inferTypes' inst = do
            (_, collectState) <- runCollect initCollectState ast (collectDefault params inst)
            subs <- unify (collected collectState)
            let types' = Map.map (applyType subs) (types inst)
            if types' == types inst then
                return inst
            else
                inferTypes'  inst{ types = types' }


infer :: ASTResolved -> [Param] -> Type -> InstBuilderState -> Except Error InstBuilderState
infer ast params retty state = do
    state' <- inferDefaults ast params retty =<< inferTypes ast params retty state
    if types state' == types state then
        return state'
    else
        infer ast params retty state'

