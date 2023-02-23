{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IRGen where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Symbol
import Monad
import Error
import Type
import Interop
import States


prettyIrGenState :: IRGenState -> IO ()
prettyIrGenState irGenState = do
    putStrLn $ "module: " ++ irModuleName irGenState
    forM_ (Map.toList $ irTypeDefs irGenState) $ \(symbol, anno) -> do
        putStrLn $ "type: " ++ show symbol ++ "\t" ++ show anno

    forM_ (Map.toList $ irCtorDefs irGenState) $ \(symbol, (t, i)) -> do
        putStrLn $ "ctor: " ++ show symbol ++ "\t" ++ show t

    forM_ (Map.toList $ irExternDefs irGenState) $ \(name, (pts, sym, ats, rt)) -> do
        putStrLn $ "extern: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt ++ " " ++ show name

    when (isJust $ irMainDef irGenState) $ do
        putStrLn $ "main: " ++ (show $ length $ funcStmts $ fromJust $ irMainDef irGenState)

    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, body) -> do
        putStrLn $ "func: " ++ show symbol
        putStrLn $ "    params: " ++ AST.brcStrs (map show $ funcParams body)
        putStrLn $ "    args:   " ++ AST.tupStrs (map show $ funcArgs body)
        putStrLn ""


initIRGenState moduleName = IRGenState
    { irModuleName     = moduleName
    , irTupleFields    = Map.empty
    , irTypeDefs       = Map.empty
    , irExternDefs     = Map.empty
    , irFuncDefs       = Map.empty
    , irCtorDefs       = Map.empty
    , irFuncMap        = Map.empty
    , irTypeMap        = Map.empty
    , irCtorMap        = Map.empty
    , irMainDef        = Nothing
    , irCurrentFunc    = ([], "", [], Void)
    }


compile :: BoM IRGenState m => ResolvedAst -> m ()
compile ast = do
    modify $ \s -> s { irTypeDefs = Map.union (typeDefs ast) (typeImports ast) }
    modify $ \s -> s { irExternDefs = funcImports ast }
    modify $ \s -> s { irCtorDefs = Map.union (ctorImports ast) (ctorDefs ast) }
    modify $ \s -> s { irTypeMap  = Map.mapKeys sym $ Map.mapWithKey (\k a -> k) (typeDefs ast) }
    modify $ \s -> s { irCtorMap  = Map.mapKeys sym $ Map.mapWithKey (\k a -> k) (ctorDefs ast) }

    initialiseTupleMembers =<< gets irCtorDefs
    initialiseTopFuncDefs (funcDefs ast)
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        compileFuncDef symbol body


initialiseTopFuncDefs :: BoM IRGenState m => Map.Map Symbol FuncBody -> m ()
initialiseTopFuncDefs funcDefs = do
    forM_ (Map.toList funcDefs) $ \(symbol, funcBody) ->
        case sym symbol of
            "main" -> do
                Nothing <- gets irMainDef
                modify $ \s -> s { irMainDef = Just (funcBody { funcStmts = [] }) }
            _ -> do
                False <- Map.member symbol <$> gets irFuncDefs
                let key = (map typeof (funcParams funcBody), sym symbol, map typeof (funcArgs funcBody), (funcRetty funcBody))
                modify $ \s -> s { irFuncMap = Map.insert  key symbol (irFuncMap s) }
                modify $ \s -> s { irFuncDefs = Map.insert symbol (funcBody { funcStmts = [] }) (irFuncDefs s) }


initialiseTupleMembers :: BoM IRGenState m => Map.Map Symbol (Type, Int) -> m ()
initialiseTupleMembers ctorDefs = do
    forM_ (Map.toList ctorDefs) $ \(symbol, (Type.Typedef symbolType, i)) -> do
        Just typ <- Map.lookup symbolType <$> gets irTypeDefs
        case typ of
            Type.Tuple ts -> modify $ \s -> s { irTupleFields = Map.insert (symbolType, sym symbol) symbol (irTupleFields s) }
            _-> return ()


compileFuncDef :: BoM IRGenState m => Symbol -> FuncBody -> m ()
compileFuncDef symbol body = do
    let paramTypes = map typeof (funcParams body)
    let argTypes   = map typeof (funcArgs body)
    let key        = (paramTypes, sym symbol, argTypes, funcRetty body)

    oldCurrentFunc <- gets irCurrentFunc
    modify $ \s -> s { irCurrentFunc = key }
    stmts' <- mapM compileStmt (funcStmts body)
    let funcBody = body { funcStmts  = stmts' }
    case sym symbol of 
        "main" -> modify $ \s -> s { irMainDef = Just funcBody }
        _      -> modify $ \s -> s { irFuncDefs = Map.insert symbol funcBody (irFuncDefs s) }
    modify $ \s -> s { irCurrentFunc = oldCurrentFunc }


exprTypeOf :: AST.Expr -> Type
exprTypeOf (AST.AExpr typ _) = typ


-- add extern if needed
resolveFuncCall :: BoM IRGenState m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = withPos pos $ do
    let paramTypes = map exprTypeOf params
    let argTypes   = map exprTypeOf args
    let key        = (paramTypes, sym symbol, argTypes, exprType)
    curModName <- gets irModuleName
    case symbol of
        SymResolved _ _ _ -> do 
            return symbol

        SymQualified mod sym -> do
            resm <- findQualifiedFuncDef mod key
            assert (isJust resm) $ "no definition for: " ++ show key
            let symbol = fromJust resm
            return symbol

        Sym sym -> do
            funcresm <- Map.lookup key <$> gets irFuncMap
            typeresm <- Map.lookup sym <$> gets irTypeMap
            case (funcresm, typeresm) of
                (Just x, Nothing) -> return x
                (Nothing, Just x) -> return x
                (Nothing, Nothing) -> do
                    funcresm <- findImportedFuncDef key
                    typeresm <- findImportedTypeDef sym
                    case (funcresm, typeresm) of
                        (Just x, Nothing) -> return x
                        (Nothing, Just x) -> return x
                        (Nothing, Nothing) -> fail $ "no def for: " ++ sym ++ " " ++ show key
    where
        findImportedFuncDef :: BoM IRGenState m => FuncKey -> m (Maybe Symbol)
        findImportedFuncDef key@(pts, sym, ats, rt) = do
            xs <- Map.toList . Map.filter (== key) <$> gets irExternDefs
            case xs of
                []              -> return (Nothing)
                [(symbol, key)] -> return (Just symbol)
                _               -> fail $ "ambiguous function call: " ++ sym

        findImportedTypeDef :: BoM IRGenState m => String -> m (Maybe Symbol)
        findImportedTypeDef sym = do
            xs <- Map.toList . Map.filterWithKey (\k a -> Symbol.sym k == sym) <$> gets irTypeDefs
            case xs of
                []               -> return (Nothing)
                [(symbol, anno)] -> return (Just symbol)
                _                -> fail $ "ambiguous type: " ++ sym
        
        findQualifiedFuncDef :: BoM IRGenState m => String -> FuncKey -> m (Maybe Symbol)
        findQualifiedFuncDef mod key = do
            xs <- Map.toList . Map.filterWithKey (\k a -> Symbol.mod k == mod && a == key) <$> gets irExternDefs
            case xs of
                []              -> return (Nothing)
                [(symbol, key)] -> return (Just symbol)
                _               -> fail $ "ambiguous function call: " ++ show key


compileStmt :: BoM IRGenState m => AST.Stmt -> m Stmt
compileStmt stmt = withPos stmt $ case stmt of
    AST.Block stmts      -> Block <$> mapM compileStmt stmts
    AST.ExprStmt expr    -> ExprStmt <$> compileExpr expr
    AST.Return pos mexpr -> Return pos <$> maybe (return Nothing) (fmap Just . compileExpr) mexpr

    AST.Typedef pos symbol anno -> do
        return $ AST.Typedef pos symbol anno

    AST.Assign pos pat expr -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        return $ Assign pos pat' expr'
    
    AST.If pos expr stmt melse -> do
        expr' <- compileExpr expr
        stmt' <- compileStmt stmt
        melse' <- maybe (return Nothing) (fmap Just . compileStmt) melse
        return $ If pos expr' stmt' melse'

    AST.While pos expr stmt -> do
        expr' <- compileExpr expr
        stmt' <- compileStmt stmt
        return $ While pos expr' stmt'

    AST.Set pos expr1 expr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- compileExpr expr2
        return $ Set pos expr1' expr2'

    AST.Print pos exprs -> do
        exprs' <- mapM compileExpr exprs
        return $ Print pos exprs'

    AST.Switch pos expr cases -> do
        expr' <- compileExpr expr
        cases' <- forM cases $ \(pat, stmt) -> do
            pat' <- compilePattern pat
            stmt' <- compileStmt stmt
            return (pat', stmt')
        return $ Switch pos expr' cases'
    
    AST.For pos expr mpat blk -> do
        expr' <- compileExpr expr
        mpat' <- maybe (return Nothing) (fmap Just . compilePattern) mpat
        blk' <- compileStmt blk
        return $ For pos expr' mpat' blk'

    AST.Data pos symbol typ mexpr -> do
        mexpr' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr
        return $ Data pos symbol typ mexpr'


compileExpr :: BoM IRGenState m => AST.Expr -> m Expr
compileExpr (AST.AExpr exprType expr) = withPos expr $ AExpr exprType <$> case expr of
    AST.Ident pos symbol      -> return $ Ident pos symbol
    AST.Prefix pos op expr    -> Prefix pos op <$> compileExpr expr
    AST.Char pos c            -> return $ AST.Char pos c
    AST.Int pos n             -> return $ Int pos n
    AST.Bool pos b            -> return $ AST.Bool pos b
    AST.Float pos f           -> return $ Float pos f
    AST.Tuple pos exprs       -> AST.Tuple pos <$> mapM compileExpr exprs
    AST.Initialiser pos exprs -> AST.Initialiser pos <$> mapM compileExpr exprs
    AST.String pos s          -> return $ AST.String pos s

    AST.Builtin pos params sym exprs -> do
        params' <- mapM compileExpr params
        exprs' <- mapM compileExpr exprs
        return $ Builtin pos params' sym exprs'

    AST.Call pos params symbol exprs -> do
        params' <- mapM compileExpr params
        exprs' <- mapM compileExpr exprs
        symbol' <- resolveFuncCall exprType expr
        return $ Call pos params' symbol' exprs'

    AST.Infix pos op expr1 expr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- compileExpr expr2
        return $ Infix pos op expr1' expr2'

    AST.Subscript pos expr1 expr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- compileExpr expr2
        return $ Subscript pos expr1' expr2'

    AST.Conv pos typ exprs -> do
        exprs' <- mapM compileExpr exprs
        return $ Conv pos typ exprs'

    AST.Field pos expr (Sym sym) -> do
        let Type.Typedef typeSymbol = exprTypeOf expr
        Just symbol' <- Map.lookup (typeSymbol, sym) <$> gets irTupleFields
        expr' <- compileExpr expr
        return $ Field pos expr' symbol'

    AST.AExpr typ expr -> do
        expr' <- compileExpr expr
        return $ AExpr typ expr'

    AST.Null pos -> return (Null pos)

    AST.ADT pos expr -> AST.ADT pos <$> compileExpr expr

    AST.Match pos expr pat -> do
        expr' <- compileExpr expr
        pat' <- compilePattern pat
        return $ Match pos expr' pat'

    AST.Range pos mexpr mexpr1 mexpr2 -> do
        mexpr' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr
        mexpr1' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr1
        mexpr2' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr2
        return $ AST.Range pos mexpr' mexpr1' mexpr2'


compilePattern :: BoM IRGenState m => AST.Pattern -> m Pattern
compilePattern pattern = case pattern of
    AST.PatIgnore pos -> return $ PatIgnore pos
    AST.PatIdent pos symbol -> do
        return $ PatIdent pos symbol

    AST.PatField pos symbol pats -> do
        pats' <- mapM compilePattern pats
        return $ PatField pos symbol pats'

    AST.PatTypeField pos typ pat -> do
        pat' <- compilePattern pat
        return $ PatTypeField pos typ pat'

    AST.PatTuple pos pats -> PatTuple pos <$> mapM compilePattern pats

    AST.PatLiteral expr -> PatLiteral <$> compileExpr expr

    AST.PatGuarded pos pat expr mpat -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        mpat' <- maybe (return Nothing) (fmap Just . compilePattern) mpat
        return $ PatGuarded pos pat' expr' mpat'

    AST.PatArray pos patss -> PatArray pos <$> mapM (mapM compilePattern) patss

    AST.PatAnnotated pat typ -> do
        pat' <- compilePattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos


