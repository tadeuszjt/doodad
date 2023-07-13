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
        }


initGenerateState
    = GenerateState
        { tuples = Map.empty
        , supply = Map.empty
        }

class (MonadBuilder m, MonadFail m, MonadState GenerateState m) => MonadGenerate m


newtype GenerateT m a = GenerateT { unGenerateT :: StateT GenerateState (StateT BuilderState m) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadGenerate)


instance Monad m => MonadBuilder (GenerateT m) where
    --liftBuilderState :: State BuilderState a -> m a
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
    n <- case nm of
        Nothing -> return 0
        Just x  -> return x
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    case n of
        0 -> return suggestion
        n -> return $ suggestion ++ show n


getStruct :: MonadGenerate m => C.Type -> m C.Type
getStruct typ = do
    sm <- Map.lookup typ <$> gets tuples
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- freshName "tuple"
            newTypedef typ name
            modify $ \s -> s { tuples = Map.insert typ name (tuples s) }
            return $ Ctypedef name
            

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
    Type.Tuple ts -> do
        cts <- mapM cTypeOf ts
        raw <- return $ Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] cts
        getStruct raw

    --Table ts -> Cstruct (C.Param "len" Cint64_t : zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map (Cpointer . cTypeOf) ts))
    --Sparse ts -> Cstruct [C.Param "elems" (cTypeOf (Table ts)), C.Param "free" (cTypeOf (Table [I64]))]
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

    Type.Tuple ts -> do
        append =<< newElement (C.ExprStmt $ C.Call "putchar" [C.Char '('])
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") $ Value t $ C.Member (valExpr val) ("m" ++ show i)
        append =<< newElement (C.ExprStmt $ C.Call "printf" [C.String (")" ++ app)])


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


    S.ExprStmt (AExpr _ (S.Builtin _ [] "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.Set _ (S.AExpr typ (S.Ident _ symbol)) expr -> do
        val <- generateExpr expr
        append =<< newElement (C.Set (show symbol) (valExpr val))

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



generateExpr :: MonadGenerate m => Expr -> m Value
generateExpr (AExpr typ expr) = case expr of
    S.Bool _ b -> return $ Value typ (C.Bool b)

    S.Int _ n -> return $ Value typ (C.Int n)

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

    _ -> error (show expr)


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
            _ -> error (show op)


