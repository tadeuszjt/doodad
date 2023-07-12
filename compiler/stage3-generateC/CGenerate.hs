{-# LANGUAGE FlexibleContexts #-}
module CGenerate where

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


data Value
    = Value { valType :: Type.Type, valExpr :: C.Expression }
    deriving (Show, Eq)

instance Typeof Value where
    typeof (Value t _) = t


cParamOf :: S.Param -> C.Param
cParamOf param = C.Param { C.cName = show (paramName param), C.cType = cTypeOf (paramType param) }

cTypeOf :: Type.Type -> C.Type
cTypeOf typ = case typ of
    I64 -> Cint64_t
    I32 -> Cint32_t
    I8 -> Cint8_t
    F64 -> Cdouble
    F32 -> Cfloat
    Type.Bool -> Cbool
    Type.Char -> Cchar
    Type.ADT fs -> Cstruct [C.Param "en" Cint64_t, C.Param "" $ Cunion $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map (cTypeOf . fieldType) fs)]
    Type.Tuple ts -> Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map cTypeOf ts)
    Void -> Cvoid
    Type.Typedef s -> Ctypedef (show s)
    Table ts -> Cstruct (C.Param "len" Cint64_t : zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map (Cpointer . cTypeOf) ts))
    Sparse ts -> Cstruct [C.Param "elems" (cTypeOf (Table ts)), C.Param "free" (cTypeOf (Table [I64]))]
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



generate :: BoM C.BuilderState m => ResolvedAst -> m ()
generate ast = do
    let typedefs = Map.union (typeImports ast) (typeDefs ast)
    orderedSymbols <- getSymbolsOrderedByDependencies typedefs
    forM_ orderedSymbols $ \symbol -> do
        when (Map.member symbol typedefs) $ do
            void $ newTypedef (cTypeOf $ typedefs Map.! symbol) (show symbol)
            
    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, funcKey@(pts, s, ats, rt)) -> case symbol of
        SymResolved _ _ _ -> do
            newExtern (show symbol) (cTypeOf rt) (map (Cpointer . cTypeOf) pts ++ map cTypeOf ats)
        _ -> return ()

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        generateFunc symbol func



generateFunc :: BoM C.BuilderState m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    let args = map cParamOf (States.funcArgs body)
    let params = map (\(C.Param n t) -> C.Param n (Cpointer t)) $ map cParamOf (States.funcParams body)
    let rettyType = cTypeOf (States.funcRetty body)
    setCurrentId =<< newFunction rettyType (show symbol) (params ++ args)
    mapM_ generateStmt (States.funcStmts body)


generateStmt :: BoM C.BuilderState m => S.Stmt -> m ()
generateStmt stmt = withPos stmt $ case stmt of
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
            case valType val of
                Type.Bool -> do
                    id <- newElement $ C.ExprStmt $ C.Call "printf" [
                        C.String (if end then "%s\n" else "%s, "),
                        C.CndExpr (valExpr val) (C.String "true") (C.String "false")
                        ]
                    append id

                Type.I64 -> do
                    id <- newElement $ C.ExprStmt $ C.Call "printf" [
                        C.String (if end then "%d\n" else "%d, "), valExpr val
                        ]
                    append id

    _ -> error (show stmt)


generatePattern :: BoM C.BuilderState m => Pattern -> Value -> m C.Expression
generatePattern pattern val = do
    case pattern of
        PatIdent _ str -> do 
            append =<< newElement (C.Assign (cTypeOf (typeof val)) (show str) (valExpr val))
            return (C.Bool True)

        PatLiteral expr -> do
            v <- generateExpr expr
            valExpr <$> generateInfix S.EqEq v val


generateExpr :: BoM C.BuilderState m => Expr -> m Value
generateExpr (AExpr typ expr) = withPos expr $ case expr of
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

    _ -> error (show expr)


generateInfix :: BoM C.BuilderState m => S.Operator -> Value -> Value -> m Value
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


