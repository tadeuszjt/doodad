{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CGenerate where

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


data GenerateState
    = GenerateState
        { tuples :: Map.Map C.Type String
        , supply :: Map.Map String Int
        , ctors  :: Map.Map Symbol (Type.Type, Int)
        , typedefs :: Map.Map Symbol Type.Type
        }

initGenerateState
    = GenerateState
        { tuples = Map.empty
        , supply = Map.empty
        , ctors  = Map.empty
        , typedefs = Map.empty
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
    Type.ADT fs -> return (typeof a)
    _ -> error (show $ typeof a)
        



data Value
    = Value { valType :: Type.Type, valExpr :: C.Expression }
    deriving (Show, Eq)

instance Typeof Value where
    typeof (Value t _) = t



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

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        generateFunc symbol func



generateFunc :: MonadGenerate m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    args <- mapM cParamOf (States.funcArgs body)
    params <- map (\(C.Param n t) -> C.Param n (Cpointer t)) <$> mapM cParamOf (States.funcParams body)
    rettyType <- cTypeOf (States.funcRetty body)
    id <- newFunction rettyType (show symbol) (params ++ args)
    withCurID id $ mapM_ generateStmt (States.funcStmts body)
    withCurID globalID $ append id



generatePrint :: MonadGenerate m => String -> Value -> m ()
generatePrint app val = case valType val of
    Type.Bool -> do
        id <- newElement $ C.ExprStmt $ C.Call "printf" [
            C.String ("%s" ++ app),
            C.CndExpr (valExpr val) (C.String "true") (C.String "false")
            ]
        append id

    Type.I64 -> do
        id <- newElement $ C.ExprStmt $ C.Call "printf" [
            C.String ("%d" ++ app), valExpr val
            ]
        append id

    Type.F64 -> do
        id <- newElement $ C.ExprStmt $ C.Call "printf" [
            C.String ("%f" ++ app), valExpr val
            ]
        append id

    Type.Tuple ts -> do
        append =<< newElement (C.ExprStmt $ C.Call "putchar" [C.Char '('])
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") $ Value t $ C.Member (valExpr val) ("m" ++ show i)
        append =<< newElement (C.ExprStmt $ C.Call "printf" [C.String (")" ++ app)])

    Type.String -> do
        id <- newElement $ C.ExprStmt $ C.Call "printf" [
            C.String ("%s" ++ app), valExpr val
            ]
        append id

    Type.Char -> do
        id <- newElement $ C.ExprStmt $ C.Call "printf" [
            C.String ("%c" ++ app), valExpr val
            ]
        append id

    _ -> error (show $ valType val)


generateStmt :: MonadGenerate m => S.Stmt -> m ()
generateStmt stmt = case stmt of
    S.Return _ (Just expr) -> do
        append =<< newElement . C.Return . valExpr =<< generateExpr expr

    S.Block stmts -> mapM_ generateStmt stmts

    S.Assign _ pattern expr -> do
        val <- generateExpr expr
        matched <- generatePattern pattern val
        return ()

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        ifID <- newElement $ C.If { ifExpr = valExpr val, ifStmts = [] }
        append ifID
        withCurID ifID $ generateStmt blk
        when (isJust melse) $ do
            elseID <- newElement $ C.Else { elseStmts = [] }
            append elseID
            withCurID elseID $ generateStmt (fromJust melse)

    S.Switch _ cnd cases -> do
        val <- generateExpr cnd

        switchId <- newElement $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
        caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
        withCurID switchId $ append caseId

        forM_ cases $ \(pattern, stmt) -> do
            b <- withCurID caseId $ generatePattern pattern val
            ifId <- newElement $ C.If { ifExpr = b, ifStmts = [] }
            withCurID ifId $ do
                generateStmt stmt
                append =<< newElement C.Break
            withCurID caseId $ append ifId

        withCurID caseId $ append =<< newElement (C.ExprStmt (C.Call "assert" [C.Int 0]))

        append switchId

    S.ExprStmt (AExpr Void (S.Call _ [] symbol exprs)) -> do
        vals <- mapM generateExpr exprs
        append =<< newElement (C.ExprStmt $ C.Call (show symbol) (map valExpr vals))


    S.ExprStmt (AExpr _ (S.Builtin _ [] "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.Set _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        append =<< newElement (C.Set (valExpr val1) (valExpr val2))

    S.For _ expr mpat stmt -> do
        idxType <- case typeof expr of
            Type.Range I64 -> cTypeOf I64
            Type.String    -> cTypeOf I64

        idxName <- freshName "index"
        append =<< newElement (C.Assign idxType idxName $ C.Int 0)

        firstName <- freshName "isFirst"
        append =<< newElement (C.Assign Cbool firstName $ C.Bool True)


        id <- newElement $ C.For Nothing Nothing (Just $ C.Increment (C.Ident idxName)) []
        append id
        withCurID id $ do
            val <- generateExpr expr
            -- special preable for ranges
            case typeof expr of
                Type.Range I64 -> do
                    setIdxId   <- newElement $ C.Set (C.Ident idxName) (C.Member (valExpr val) "min")
                    setFirstId <- newElement $ C.Set (C.Ident firstName) (C.Bool False)
                    append =<< newElement (C.If
                        { ifExpr = C.Ident firstName
                        , ifStmts = [setFirstId, setIdxId]
                        })

                Type.String -> return ()

            -- check that index is still in range
            case typeof expr of
                Type.Range I64 -> do
                    breakId <- newElement C.Break
                    ifId <- newElement $ C.If
                        { ifExpr = C.Infix C.GTEq (C.Ident idxName) (C.Member (valExpr val) "max")
                        , ifStmts = [breakId]
                        }
                    append ifId
                Type.String -> do
                    breakId <- newElement C.Break
                    ifId <- newElement $ C.If
                        { ifExpr = C.Infix C.GTEq (C.Ident idxName) (C.Call "strlen" [valExpr val])
                        , ifStmts = [breakId]
                        }
                    append ifId
                    
            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return (C.Bool True)
                Just pat -> case typeof expr of
                    Type.Range I64 -> generatePattern pat (Value I64 $ C.Ident idxName)
                    Type.String    -> generatePattern pat (Value Type.Char $ C.Subscript (valExpr val) (C.Ident idxName))
            ifId <- newElement (C.If { ifExpr = patMatches , ifStmts = [] })
            append ifId
            withCurID ifId $ generateStmt stmt
            breakId <- newElement C.Break
            append =<< newElement (C.Else { elseStmts = [breakId] })

    _ -> error (show stmt)


generatePattern :: MonadGenerate m => Pattern -> Value -> m C.Expression
generatePattern pattern val = do
    case pattern of
        PatIdent _ str -> do 
            cType <- cTypeOf (typeof val)
            append =<< newElement (C.Assign cType (show str) (valExpr val))
            return (C.Bool True)

        PatLiteral expr -> do
            v <- generateExpr expr
            valExpr <$> generateInfix S.EqEq v val

        PatIgnore _ -> return (C.Bool True)

        PatGuarded _ pat expr Nothing -> do -- TODO
            b <- generatePattern pat val
            match <- freshName "match"
            append =<< newElement (C.Assign Cbool match b)
            ifId <- newElement $ C.If { ifExpr = b, ifStmts = [] }
            withCurID ifId $ do
                v <- generateExpr expr
                append =<< newElement (C.Set (C.Ident match) (valExpr v))
            append ifId
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
                    append =<< newElement (C.Assign Cbool match $
                        C.Infix C.EqEq (C.Int $ fromIntegral i) (C.Member (valExpr val) "en"))

                    return (C.Ident match)



generateExpr :: MonadGenerate m => Expr -> m Value
generateExpr (AExpr typ expr) = case expr of
    S.Bool _ b -> return $ Value typ (C.Bool b)

    S.Int _ n -> return $ Value typ (C.Int n)

    S.Float _ f -> return $ Value typ (C.Float f)

    S.Ident _ symbol -> return $ Value typ (C.Ident $ show symbol)

    S.Infix _ op a b -> do
        valA <- generateExpr a
        valB <- generateExpr b
        generateInfix op valA valB

    S.Call _ [] symbol args -> do
        vals <- mapM generateExpr args
        return $ Value typ $ C.Call (show symbol) (map valExpr vals)

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        cType <- cTypeOf typ
        name <- freshName "tuple"
        id <- newElement $ C.Assign cType name (C.Initialiser $ map valExpr vals)
        append id
        return $ Value typ $ C.Ident name

    S.Range _ Nothing (Just expr1) (Just expr2) -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        assert (typeof val1 == typeof val2) "type mismatch"
        ctype <- cTypeOf typ
        name <- freshName "range"
        id <- newElement $ C.Assign ctype name (C.Initialiser [valExpr val1, valExpr val2])
        append id
        return $ Value typ $ C.Ident name

    S.String _ s -> do
        return $ Value typ $ C.String s

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
                        append =<< newElement (C.Assign ctyp name (C.Initialiser [C.Int $ fromIntegral i])) -- TODO
                        return $ Value typ $ C.Ident name

            _ -> error (show typ)

    S.Conv pos typ [] -> do -- construct 0
        name <- freshName "zero"
        ctyp <- cTypeOf typ
        append =<< newElement (C.Assign ctyp name (C.Initialiser [C.Int 0]))
        return $ Value typ (C.Ident name)

    S.Subscript _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        return $ Value typ $ C.Subscript (C.Member (valExpr val1) "arr") (valExpr val2)

    _ -> error (show expr)
generateExpr x = error (show x)


generateInfix :: MonadGenerate m => S.Operator -> Value -> Value -> m Value
generateInfix op a b = do
    assert (typeof a == typeof b) "infix types do not match"
    case typeof a of
        Type.I64 -> return $ Value (typeof a) $ case op of
            S.Plus ->  C.Infix C.Plus (valExpr a) (valExpr b) 
            S.Times -> C.Infix C.Times (valExpr a) (valExpr b) 
            S.LTEq ->  C.Infix C.LTEq (valExpr a) (valExpr b)
            S.EqEq ->  C.Infix C.EqEq (valExpr a) (valExpr b)
            S.Minus -> C.Infix C.Minus (valExpr a) (valExpr b)
            S.Modulo -> C.Infix C.Modulo (valExpr a) (valExpr b)
            _ -> error (show op)

        Type.String -> case op of
            S.Plus -> return $ Value (typeof a) (C.Call "doodad_string_plus" [valExpr a, valExpr b])


