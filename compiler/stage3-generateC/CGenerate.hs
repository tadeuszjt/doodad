{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CGenerate where

import Data.Char
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Monad
import Symbol
import States
import CBuilder as C
import CAst as C
import Control.Monad.State
import Type
import AST as S
import Error

data Object
    = Pointer Type.Type C.Expression
    | Value   Type.Type C.Expression
    deriving (Show, Eq)

instance Typeof Object where
    typeof (Value t _) = t
    typeof (Pointer t _) = t

valueExpr :: Object -> C.Expression
valueExpr object = case object of
    Pointer t e -> C.Deref (e)
    Value t e -> e

pointerExpr :: Object -> C.Expression
pointerExpr obj = case obj of
    Pointer t e -> e
    Value t e   -> C.Address e


data GenerateState
    = GenerateState
        { tuples :: Map.Map C.Type String
        , supply :: Map.Map String Int
        , ctors  :: Map.Map Symbol (Type.Type, Int)
        , typedefs :: Map.Map Symbol Type.Type
        , symTab :: Map.Map String Object
        }

initGenerateState
    = GenerateState
        { tuples = Map.empty
        , supply = Map.empty
        , ctors  = Map.empty
        , typedefs = Map.empty
        , symTab = Map.empty
        }


class (MonadBuilder m, MonadFail m, MonadState GenerateState m) => MonadGenerate m

newtype GenerateT m a = GenerateT { unGenerateT :: StateT GenerateState (StateT BuilderState m) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadGenerate)

instance Monad m => MonadBuilder (GenerateT m) where
    liftBuilderState (StateT s) = GenerateT $ lift $ StateT $ pure . runIdentity . s

instance MonadTrans GenerateT where
    lift = GenerateT . lift . lift 

instance Monad m => MonadFail (GenerateT m) where
    fail = error



define :: MonadGenerate m => String -> Object -> m ()
define str obj = do
    isDefined <- Map.member str <$> gets symTab
    assert (not isDefined) $ str ++ " already defined"
    modify $ \s -> s { symTab = Map.insert str obj (symTab s) }


look :: MonadGenerate m => String -> m Object
look str = (Map.! str) <$> gets symTab



runGenerateT :: Monad m => GenerateState -> BuilderState -> GenerateT m a -> m ((a, GenerateState), BuilderState)
runGenerateT generateState builderState generateT =
    runStateT (runStateT (unGenerateT generateT) generateState) builderState


freshName :: MonadGenerate m => String -> m String
freshName suggestion = do
    nm <- Map.lookup suggestion <$> gets supply
    let n = maybe 0 id nm
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    return $ suggestion ++ "__" ++ show n

getTypedef :: MonadGenerate m => C.Type -> m C.Type
getTypedef typ = do
    sm <- Map.lookup typ <$> gets tuples
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- freshName "struct"
            newTypedef typ name
            modify $ \s -> s { tuples = Map.insert typ name (tuples s) }
            return $ Ctypedef name
            
baseTypeOf :: (MonadGenerate m, Typeof a) => a -> m Type.Type
baseTypeOf a = case typeof a of
    Type.Typedef s -> baseTypeOf . (Map.! s) =<< gets typedefs
    _ -> return (typeof a)
    _ -> error (show $ typeof a)


cParamOf :: MonadGenerate m => S.Param -> m C.Param
cParamOf param = do
    ctype <- cTypeOf (paramType param)
    return $ C.Param { C.cName = show (paramName param), C.cType = ctype }


cTypeOf :: MonadGenerate m => Type.Type -> m C.Type
cTypeOf typ = case typ of
    I64 -> return $ Cint64_t
    I32 -> return $ Cint32_t
    I8 ->  return $ Cint8_t
    F64 -> return $ Cdouble
    F32 -> return $ Cfloat
    Void -> return Cvoid
    Type.Bool -> return $ Cbool
    Type.Char -> return $ Cchar
    Type.Typedef s -> return $ Ctypedef (show s)
    Type.String -> return $ Cpointer Cchar
    Type.Array n t -> do
        arr <- Carray n <$> cTypeOf t
        getTypedef $ Cstruct [C.Param "arr" arr]
    Type.Tuple ts -> do
        cts <- mapM cTypeOf ts
        getTypedef $ Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] cts
    Type.Range t -> do
        ct <- cTypeOf t
        getTypedef $ Cstruct [C.Param "min" ct, C.Param "max" ct]
    Type.ADT fs -> do
        cts <- mapM cTypeOf (map fieldType fs)
        getTypedef $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
            Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

    _ -> error (show typ)
    where
        fieldType f = case f of
            FieldNull -> I8
            FieldType t -> t
            FieldCtor [t] -> t
            FieldCtor ts -> Type.Tuple ts
        

getSymbolsOrderedByDependencies :: Monad m => Map.Map Symbol Type.Type -> m [Symbol]
getSymbolsOrderedByDependencies typedefs = do
    fmap (removeDuplicates . concat) . forM (Map.toList typedefs) $ \(s, t) -> do
        symbols <- getSymbols t
        return (symbols ++ [s])
    where
        getSymbols :: Monad m => Type.Type -> m [Symbol]
        getSymbols typ = case typ of
            Type.Typedef s -> return [s]
            Type.Tuple ts  -> concat <$> mapM getSymbols ts
            Table ts  -> concat <$> mapM getSymbols ts
            Sparse ts  -> concat <$> mapM getSymbols ts
            Type.Array n t -> getSymbols t
            I64 -> return []
            I32 -> return []
            F64 -> return []
            F32 -> return []
            Type.ADT fs -> concat <$> mapM getSymbolsField fs
            Type.Bool -> return []
            _ -> error (show typ)

        getSymbolsField :: Monad m => AdtField -> m [Symbol]
        getSymbolsField field = case field of
            FieldNull -> return []
            FieldType t -> getSymbols t
            FieldCtor ts -> concat <$> mapM getSymbols ts
            _ -> error (show field)
        
        removeDuplicates :: Eq a => [a] -> [a]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | elem x xs = x:removeDuplicates (deleteAll x xs)
            | otherwise = x:removeDuplicates xs

        deleteAll :: Eq a => a -> [a] -> [a]
        deleteAll a [] = []
        deleteAll a (x:xs)
            | a == x    = deleteAll a xs
            | otherwise = x : deleteAll a xs



generate :: MonadGenerate m => ResolvedAst -> m ()
generate ast = do
    let typedefs = Map.union (typeImports ast) (typeDefs ast)

    -- copy ctors
    modify $ \s -> s { ctors = Map.union (ctorDefs ast) (ctorImports ast) }
    modify $ \s -> s { typedefs = Map.union (typeDefs ast) (typeImports ast) }

    orderedSymbols <- getSymbolsOrderedByDependencies typedefs
    forM_ orderedSymbols $ \symbol -> do
        when (Map.member symbol typedefs) $ do
            ctype <- cTypeOf (typedefs Map.! symbol)
            void $ newTypedef ctype (show symbol)
            
    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, funcKey@(pts, s, ats, rt)) -> case symbol of
        SymResolved _ _ _ -> do
            crt <- cTypeOf rt
            cpts <- map Cpointer <$> mapM cTypeOf pts
            cats <- mapM cTypeOf ats
            newExtern (show symbol) crt (cpts ++ cats)
        _ -> return ()

    -- generate function headers
    
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        crt <- cTypeOf (States.funcRetty func)
        cpts <- map Cpointer <$> mapM cTypeOf (map paramType $ States.funcParams func)
        cats <- mapM cTypeOf (map paramType $ States.funcArgs func)
        newExtern (show symbol) crt (cpts ++ cats)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        generateFunc symbol func



generateFunc :: MonadGenerate m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    args <- mapM cParamOf (States.funcArgs body)
    params <- map (\(C.Param n t) -> C.Param n (Cpointer t)) <$> mapM cParamOf (States.funcParams body)
    rettyType <- cTypeOf (States.funcRetty body)

    forM_ (States.funcParams body) $ \param -> do
        ctyp <- cTypeOf (paramType param)
        name <- return $ show (S.paramName param)
        define name $ Pointer (S.paramType param) (C.Ident name)

    forM_ (States.funcArgs body) $ \arg -> do
        ctyp <- cTypeOf (paramType arg)
        name <- return $ show (S.paramName arg)
        define name $ Value (S.paramType arg) (C.Ident name)

    id <- newFunction rettyType (show symbol) (params ++ args)
    withCurID id $ mapM_ generateStmt (States.funcStmts body)
    withCurID globalID $ append id



generatePrint :: MonadGenerate m => String -> Object -> m ()
generatePrint app val = case typeof val of
    Type.I64 ->    void $ appendPrintf ("%d" ++ app) [valueExpr val]
    Type.F64 ->    void $ appendPrintf ("%f" ++ app) [valueExpr val]
    Type.String -> void $ appendPrintf ("%s" ++ app) [valueExpr val]
    Type.Char ->   void $ appendPrintf ("%c" ++ app) [valueExpr val]

    Type.Bool -> void $ appendPrintf ("%s" ++ app) $
        [C.CndExpr (valueExpr val) (C.String "true") (C.String "false")]

    Type.Tuple ts -> do
        appendElem $ C.ExprStmt $ C.Call "putchar" [C.Char '(']
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") =<< generateTupleIndex val i
        void $ appendPrintf (")" ++ app) []

    _ -> error (show $ typeof val)


generateStmt :: MonadGenerate m => S.Stmt -> m ()
generateStmt stmt = case stmt of
    S.EmbedC _ str -> do void $ appendElem (C.Embed str)
    S.Block stmts -> mapM_ generateStmt stmts

    S.Return _ (Just expr) -> do
        void $ appendElem . C.Return . valueExpr =<< generateExpr expr

    S.Assign _ pattern expr -> do
        val <- generateExpr expr
        matched <- generatePattern pattern val
        return ()

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        ifID <- appendIf (valueExpr val)
        withCurID ifID $ generateStmt blk
        when (isJust melse) $ do
            elseID <- appendElem $ C.Else { elseStmts = [] }
            withCurID elseID $ generateStmt (fromJust melse)

    S.Switch _ cnd cases -> do
        val <- generateExpr cnd

        switchId <- newElement $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
        caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
        withCurID switchId $ append caseId

        forM_ cases $ \(pattern, stmt) -> do
            ifId <- withCurID caseId $ appendIf =<< withCurID caseId (generatePattern pattern val)
            withCurID ifId $ do
                generateStmt stmt
                appendElem C.Break

        withCurID caseId $ appendElem (C.ExprStmt (C.Call "assert" [C.Int 0]))

        append switchId

    S.ExprStmt (AExpr Void (S.Call _ exprs1 symbol exprs2)) -> do
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        void $ appendElem $ C.ExprStmt $
            C.Call (show symbol) (map pointerExpr objs1 ++ map valueExpr objs2)

    S.ExprStmt (AExpr _ (S.Builtin _ [] "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.Set _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        void $ appendElem $ C.Set (valueExpr val1) (valueExpr val2)

    S.For _ expr mpat stmt -> do
        base <- baseTypeOf expr
        idxType <- case base of
            Type.Range I64 -> cTypeOf I64
            Type.String    -> cTypeOf I64
            Type.Array _ _ -> cTypeOf I64

        idxName <- freshName "index"
        appendElem (C.Assign idxType idxName $ C.Int 0)

        firstName <- freshName "isFirst"
        appendElem (C.Assign Cbool firstName $ C.Bool True)


        id <- appendElem $ C.For Nothing Nothing (Just $ C.Increment (C.Ident idxName)) []
        withCurID id $ do
            val <- generateExpr expr
            -- special preable for ranges
            case base of
                Type.Range I64 -> do
                    ifID <- appendIf (C.Ident firstName)
                    void $ withCurID ifID $ do
                        appendElem $ C.Set (C.Ident idxName) (C.Member (valueExpr val) "min")
                        appendElem $ C.Set (C.Ident firstName) (C.Bool False)

                Type.String -> return ()
                Type.Array _ _ -> return ()

            -- check that index is still in range
            case base of
                Type.Range I64 -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Member (valueExpr val) "max")
                    withCurID ifId $ appendElem C.Break
                Type.String -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Call "strlen" [valueExpr val])
                    withCurID ifId $ appendElem C.Break
                Type.Array n t -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Int $ fromIntegral n)
                    withCurID ifId $ appendElem C.Break

                    
            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return (C.Bool True)
                Just pat -> case base of
                    Type.Range I64 -> generatePattern pat (Value I64 $ C.Ident idxName)
                    Type.String    -> generatePattern pat (Value Type.Char $ C.Subscript (valueExpr val) (C.Ident idxName))
                    Type.Array n t -> generatePattern pat (Value t $ C.Subscript (C.Member (valueExpr val) "arr") (C.Ident idxName))

            ifId <- appendIf (C.Not patMatches)
            withCurID ifId $ appendElem C.Break
            generateStmt stmt

    _ -> error (show stmt)



generateTupleIndex :: MonadGenerate m => Object -> Int -> m Object
generateTupleIndex obj i = do
    base@(Type.Tuple ts) <- baseTypeOf obj
    case obj of
        Value _ e   -> return $ Value (ts !! i) (C.Member e $ "m" ++ show i)
        Pointer _ e -> return $ Value (ts !! i) (C.PMember e $ "m" ++ show i)


generateAdtEnum :: MonadGenerate m => Object -> m Object
generateAdtEnum obj = do
    base@(Type.ADT fs) <- baseTypeOf obj
    return $ Value I64 $ C.Member (valueExpr obj) "en"


generateAdtEqual :: MonadGenerate m => Object -> Object -> m Object
generateAdtEqual a b = do
    assert (typeof a == typeof b) "types aren't equal"
    base@(Type.ADT fs) <- baseTypeOf a

    eqName <- freshName "adtEqual"
    appendElem $ C.Assign Cbool eqName (C.Bool False)
    
    enA <- generateAdtEnum a
    enB <- generateAdtEnum b
    ifId <- appendIf (C.Infix C.EqEq (valueExpr enA) (valueExpr enB))
    withCurID ifId $ do
        switchId <- appendElem $ C.Switch (valueExpr enA) []
        withCurID switchId $ do
            forM_ (zip fs [0..]) $ \(field, i) -> do
                caseId <- appendElem $ Case (C.Int i) []
                withCurID caseId $ do
                    case field of
                        FieldCtor [] -> do
                            appendElem $ C.Set (C.Ident eqName) (C.Bool True)
                            appendElem $ C.Break

    return $ Value Type.Bool $ C.Ident eqName


generatePattern :: MonadGenerate m => Pattern -> Object -> m C.Expression
generatePattern pattern val = do
    case pattern of
        PatIgnore _ -> return (C.Bool True)

        PatIdent _ symbol -> do 
            let name = show symbol
            define name (Value (typeof val) $ C.Ident name)
            cType <- cTypeOf (typeof val)
            appendElem (C.Assign cType (show symbol) (valueExpr val))
            return (C.Bool True)

        PatLiteral expr -> do
            v <- generateExpr expr
            valueExpr <$> generateInfix S.EqEq v val

        PatTuple _ pats -> do
            base@(Type.Tuple ts) <- baseTypeOf val
            assert (length ts == length pats) "length mismatch"
            bs <- forM (zip pats [0..]) $ \(pat, i) -> do
                generatePattern pat =<< generateTupleIndex val i

            name <- freshName "match"
            appendElem (C.Assign Cbool name $ foldr1 (C.Infix C.AndAnd) bs)
            return $ C.Ident name

        PatGuarded _ pat expr Nothing -> do -- TODO
            b <- generatePattern pat val
            match <- freshName "match"
            appendElem (C.Assign Cbool match b)
            ifId <- appendIf b
            withCurID ifId $ do
                v <- generateExpr expr
                appendElem (C.Set (C.Ident match) (valueExpr v))
            return (C.Ident match)

        PatField _ symbol exprs -> do -- either a typedef or an ADT field, both members of ADT
            base@(Type.ADT fs) <- baseTypeOf val

            isCtor <- Map.member symbol <$> gets ctors
            isTypedef <- Map.member symbol <$> gets typedefs

            case (isCtor, isTypedef) of
                (True, False) -> do
                    assert (exprs == []) "TODO"
                    (typ', i) <- (Map.! symbol) <$> gets ctors
                    match <- freshName "match"
                    appendElem . C.Assign Cbool match . C.Infix C.EqEq (C.Int $ fromIntegral i) . valueExpr
                        =<< generateAdtEnum val

                    return (C.Ident match)



generateExpr :: MonadGenerate m => Expr -> m Object
generateExpr (AExpr typ expr) = withTypeCheck $ case expr of
    S.Bool _ b -> return $ Value typ (C.Bool b)
    S.Int _ n -> return $ Value typ (C.Int n)
    S.Float _ f -> return $ Value typ (C.Float f)
    S.Ident _ symbol -> look (show symbol)
    S.String _ s -> return $ Value typ $ C.String s
    S.Char _ c -> return $ Value typ $ C.Char c

    S.Infix _ op a b -> do
        valA <- generateExpr a
        valB <- generateExpr b
        generateInfix op valA valB

    S.Call _ exprs1 symbol exprs2 -> do
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        return $ Value typ $ C.Call (show symbol) (map pointerExpr objs1 ++ map valueExpr objs2)

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        cType <- cTypeOf typ
        name <- freshName "tuple"
        id <- appendElem $ C.Assign cType name (C.Initialiser $ map valueExpr vals)
        return $ Value typ $ C.Ident name

    S.Builtin _ [] "len" [expr] -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        case base of
            Type.Array n t -> return $ Value typ $ C.Int (fromIntegral n)

    S.Range _ (Just expr) mexpr1 mexpr2 -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        case base of
            Type.Array n t -> do
                start <- case mexpr1 of
                    Nothing -> return $ Value Type.I64 $ C.Int 0

                end <- case mexpr2 of
                    Nothing -> return $ Value Type.I64 $ C.Int (fromIntegral n)

                ctype <- cTypeOf typ
                name <- freshName "range"
                id <- appendElem $
                    C.Assign ctype name (C.Initialiser [valueExpr start, valueExpr end])
                return $ Value typ $ C.Ident name


    S.Range _ Nothing (Just expr1) (Just expr2) -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        assert (typeof val1 == typeof val2) "type mismatch"
        ctype <- cTypeOf typ
        name <- freshName "range"
        appendElem $ C.Assign ctype name (C.Initialiser [valueExpr val1, valueExpr val2])
        return $ Value typ $ C.Ident name

    S.Construct _ symbol exprs -> do
        base <- baseTypeOf typ
        vals <- mapM generateExpr exprs
        (typ', i) <- (Map.! symbol) <$> gets ctors
        assert (typ == typ') "error, types don't match"

        case base of
            Type.ADT fs -> do
                assert (i < length fs) "invalid index"
                case fs !! i of
                    FieldCtor [] -> do
                        assert (vals == []) "TODO"
                        name <- freshName "adt"
                        ctyp <- cTypeOf typ
                        appendElem $ C.Assign ctyp name (C.Initialiser [C.Int $ fromIntegral i]) -- TODO
                        return $ Value typ $ C.Ident name

            _ -> error (show typ)

    S.Conv pos typ [] -> do -- construct 0
        name <- freshName "zero"
        ctyp <- cTypeOf typ
        appendElem $ C.Assign ctyp name (C.Initialiser [C.Int 0])
        return $ Value typ (C.Ident name)

    S.Subscript _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        base <- baseTypeOf val1
        case base of
            Type.Array n t -> do
                return $ Value typ $ C.Subscript (C.Member (valueExpr val1) "arr") (valueExpr val2)

    _ -> error (show expr)
    where
        withTypeCheck :: MonadGenerate m => m Object -> m Object
        withTypeCheck f = do
            r <- f
            assert (typeof r == typ) $ 
                "generateExpr returned: " ++ show r ++ " but checked " ++ show typ
            return r
            



generateInfix :: MonadGenerate m => S.Operator -> Object -> Object -> m Object
generateInfix op a b = do
    assert (typeof a == typeof b) "infix types do not match"
    base <- baseTypeOf a
    case base of
        Type.I64 -> return $ case op of
            S.Plus ->   Value (typeof a) $ C.Infix C.Plus (valueExpr a) (valueExpr b) 
            S.Times ->  Value (typeof a) $ C.Infix C.Times (valueExpr a) (valueExpr b) 
            S.Minus ->  Value (typeof a) $ C.Infix C.Minus (valueExpr a) (valueExpr b)
            S.Modulo -> Value (typeof a) $ C.Infix C.Modulo (valueExpr a) (valueExpr b)
            S.LT ->     Value Type.Bool $ C.Infix C.LT (valueExpr a) (valueExpr b)
            S.LTEq ->   Value Type.Bool $ C.Infix C.LTEq (valueExpr a) (valueExpr b)
            S.EqEq ->   Value Type.Bool $ C.Infix C.EqEq (valueExpr a) (valueExpr b)
            _ -> error (show op)

        Type.Bool -> return $ case op of
            S.AndAnd -> Value (typeof a) $ C.Infix C.AndAnd (valueExpr a) (valueExpr b)

        Type.String -> case op of
            S.Plus -> return $ Value (typeof a) (C.Call "doodad_string_plus" [valueExpr a, valueExpr b])

        Type.Array n t -> case op of
            S.EqEq -> do
                idxName <- freshName "index"
                eqName <- freshName "equal"
                appendElem $ C.Assign Cint64_t idxName (C.Int 0)
                appendElem $ C.Assign Cbool eqName (C.Bool True)
                forId <- appendElem $ C.For
                    Nothing
                    (Just (C.Infix C.LT (C.Ident idxName) (C.Int $ fromIntegral n)))
                    (Just (C.Increment $ C.Ident idxName))
                    []
                withCurID forId $ do
                    let elemA = Value t $ C.Subscript (C.Member (valueExpr a) "arr") (C.Ident idxName)
                    let elemB = Value t $ C.Subscript (C.Member (valueExpr b) "arr") (C.Ident idxName)
                    b <- generateInfix S.EqEq elemA elemB
                    ifId <- appendIf (C.Not $ valueExpr b)
                    withCurID ifId $ do
                        appendElem $ C.Set (C.Ident eqName) (C.Bool False)
                        appendElem $ C.Break
                return $ Value Type.Bool $ C.Ident eqName

        Type.ADT fs -> case op of
            S.NotEq -> do
                eq <- generateAdtEqual a b
                return $ Value Type.Bool $ (C.Not $ valueExpr eq)

            S.EqEq -> generateAdtEqual a b
                    

        _ -> error $ show base


