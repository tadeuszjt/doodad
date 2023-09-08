{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CleanUp where

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
import ASTResolved
import Apply


-- Resolves function calls
-- Creates generic instantiations
-- Resolves tuple/table field symbols
-- Turns ctor function call into Contructors


compile :: BoM ASTResolved m => m ()
compile = do
    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        stmt' <- compileStmt (funcStmt body)
        body' <- return body { funcStmt = stmt' }
        modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }


genSymbol :: BoM ASTResolved m => String -> m Symbol
genSymbol sym = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup sym . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    let symbol = SymResolved modName sym n
    return symbol


--- TODO doesn't check list lengths
getSubsFromTypes :: BoM s m => Type -> Type -> m [(Type, Type)]
getSubsFromTypes t1 t2 = case (t1, t2) of
    (Void, Void) -> return []
    (I64, I64) -> return []
    (Type.Char, Type.Char) -> return []
    (Type.String, Type.String) -> return []
    (Type.Typedef s1, Type.Typedef s2) -> return []
    (Type.Bool, Type.Bool) -> return []
    (Table ts1, Table ts2) -> concat <$> zipWithM getSubsFromTypes ts1 ts2
    (Type.Tuple ts1, Type.Tuple ts2) -> concat <$> zipWithM getSubsFromTypes ts1 ts2
    (ADT fs1, ADT fs2)     -> concat <$> zipWithM getSubsFromFields fs1 fs2
    (Type _, _) -> return []
    _ -> error $ show (t1, t2)
    where
        getSubsFromFields :: BoM s m => AdtField -> AdtField -> m [(Type, Type)]
        getSubsFromFields field1 field2 = case (field1, field2) of
            (FieldNull, FieldNull) -> return []
            (FieldType t1, FieldType t2) -> getSubsFromTypes t1 t2
            _ -> error $ show (field1, field2)


getSubsFromGeneric :: BoM s m => FuncKey -> FuncBody -> m [(Type, Type)]
getSubsFromGeneric callKey@(ps, symbol, as, rt) funcBody = do
    subsPs <- fmap concat $ forM (zip ps $ map typeof $ funcParams funcBody) $ \(p, bp) -> do
        getSubsFromTypes p bp
    subsAs <- fmap concat $ forM (zip as $ map typeof $ funcArgs funcBody) $ \(a, ba) -> do
        getSubsFromTypes a ba
    subsRt <- getSubsFromTypes rt (funcRetty funcBody)
    return $ Set.toList $ Set.fromList $ subsPs ++ subsAs ++ subsRt
    

-- add extern if needed
resolveFuncCall :: BoM ASTResolved m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = withPos pos $ do
    let key = (map typeof params, sym symbol, map typeof args, exprType)
    case symbol of
        SymResolved _ _ _ -> return symbol

        SymQualified mod sym -> do
            resm <- findQualifiedFuncDef mod key
            assert (isJust resm) $ "no definition for: " ++ show key
            return (fromJust resm)

        Sym sym -> do
            funcresm <- findFuncDef key
            typeresm <- findTypeDef sym
            case (funcresm, typeresm) of
                (Just x, Nothing) -> do
                    funcBody <- (Map.! x) <$> gets funcDefs
                    subs <- getSubsFromGeneric key funcBody
                    if (subs /= []) then do -- function is generic
                        funcBody' <- return $ applySubs subs funcBody
                        symbol' <- genSymbol sym
                        modify $ \s -> s { funcDefs = Map.insert symbol' funcBody' (funcDefs s) }
                        return symbol'
                    else do
                        return x
                (Nothing, Just x) -> return x
                (Nothing, Nothing) -> do
                    funcresm <- findImportedFuncDef key
                    case funcresm of
                        Just x -> return x
                        Nothing -> fail $ "no def for: " ++ sym ++ " " ++ show key
    where
        findFuncDef :: BoM ASTResolved m => FuncKey -> m (Maybe Symbol)
        findFuncDef key = useLast =<< Map.filterWithKey
            (\symbol body -> funcKeysCouldMatch (funcKeyFromBody (sym symbol) body) key) <$> gets funcDefs

        funcKeysCouldMatch :: FuncKey -> FuncKey -> Bool
        funcKeysCouldMatch (aps, asymbol, aas, art) (bps, bsymbol, bas, brt)
            | length aps /= length bps || length aas /= length bas = False
            | asymbol /= bsymbol = False
            | otherwise = all (== True) $
                zipWith typesCouldMatch (aps ++ aas ++ [art]) (bps ++ bas ++ [brt])


        findTypeDef :: BoM ASTResolved m => String -> m (Maybe Symbol)
        findTypeDef sym = checkOne =<< Map.filterWithKey (\s t -> Symbol.sym s == sym) <$> gets typeDefs

        findImportedFuncDef :: BoM ASTResolved m => FuncKey -> m (Maybe Symbol)
        findImportedFuncDef key = checkOne =<< Map.filter (funcKeysCouldMatch key) <$> gets funcImports

        findQualifiedFuncDef :: BoM ASTResolved m => String -> FuncKey -> m (Maybe Symbol)
        findQualifiedFuncDef mod key = checkOne =<< Map.filterWithKey (\s k -> Symbol.mod s == mod && k == key) <$> gets funcImports

        -- TODO this is very unsafe, prioritise non-generic?
        useLast :: BoM s m => Map.Map Symbol b -> m (Maybe Symbol)
        useLast mp = case Map.keys mp of
            [] -> return Nothing
            xs -> return (Just $ last xs)

        checkOne :: BoM s m => Map.Map Symbol b -> m (Maybe Symbol)
        checkOne mp = case Map.keys mp of
            [] -> return Nothing
            [symbol] -> return (Just symbol)
            _ -> fail $ "Ambiguous symbol: " ++ show symbol


compileStmt :: BoM ASTResolved m => AST.Stmt -> m Stmt
compileStmt stmt = withPos stmt $ case stmt of
    AST.EmbedC pos s     -> return (AST.EmbedC pos s)
    AST.Block stmts      -> Block <$> mapM compileStmt stmts
    AST.ExprStmt expr    -> ExprStmt <$> compileExpr expr
    AST.Return pos mexpr -> Return pos <$> maybe (return Nothing) (fmap Just . compileExpr) mexpr
    AST.FuncDef pos params symbol args retty blk -> return stmt
    AST.Const pos symbol expr -> return stmt
    AST.Typedef pos symbol anno -> return $ AST.Typedef pos symbol anno

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

    AST.SetOp pos op expr1 expr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- compileExpr expr2
        return $ SetOp pos op expr1' expr2'

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


resolveFieldAccess :: BoM ASTResolved m => AST.Expr -> m Expr
resolveFieldAccess (AST.Field pos expr (Sym sym)) = do
        -- (tup:typeSymbol).x:i64
        -- find mod_x_n
        ctors <- gets ctorDefs
        res <- Map.toList . Map.filterWithKey (\k a -> tupTypeMatches a && fieldSymMatches k)
            <$> gets ctorDefs
        case res of
            []            -> do
                expr' <- compileExpr expr
                return $ Field pos expr' (Sym sym)
            (a:b:xs)      -> fail "ambiguous"
            [(symbol, _)] -> do
                expr' <- compileExpr expr
                return $ Field pos expr' symbol
        where
            tupTypeMatches :: (Type, Int) -> Bool
            tupTypeMatches (t, _) = t == typeof expr

            fieldSymMatches :: Symbol -> Bool
            fieldSymMatches symbol = Symbol.sym symbol == sym


compileExpr :: BoM ASTResolved m => AST.Expr -> m Expr
compileExpr (AST.AExpr exprType expr) = withPos expr $ AExpr exprType <$> case expr of
    AST.Ident pos symbol      -> return $ Ident pos symbol
    AST.Prefix pos op expr    -> Prefix pos op <$> compileExpr expr
    AST.Char pos c            -> return $ AST.Char pos c
    AST.Int pos n             -> return $ Int pos n
    AST.Bool pos b            -> return $ AST.Bool pos b
    AST.Float pos f           -> return $ Float pos f
    AST.Tuple pos exprs       -> AST.Tuple pos <$> mapM compileExpr exprs
    AST.String pos s          -> return $ AST.String pos s
    AST.Array pos exprs       -> AST.Array pos <$> mapM compileExpr exprs


    AST.Field pos e symbol -> case symbol of
        Sym _ -> resolveFieldAccess expr
        SymResolved _ _ _ -> do
            e' <- compileExpr e
            return $ Field pos e' symbol

    AST.Builtin pos [] "conv" exprs -> do
        return $ Conv pos exprType exprs

    AST.Builtin pos params sym exprs -> do
        params' <- mapM compileExpr params
        exprs' <- mapM compileExpr exprs
        return $ Builtin pos params' sym exprs'

    AST.Call pos params symbol exprs -> do
        params' <- mapM compileExpr params
        exprs' <- mapM compileExpr exprs
        symbol' <- resolveFuncCall exprType expr

        isCtor <- Map.member symbol' <$> gets ctorDefs
        if isCtor then do
            assert (params' == []) "constructor cannot have params"
            return $ Construct pos symbol' exprs'
        else do
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

    AST.Construct pos symbol@(SymResolved _ _ _) exprs -> do
        exprs' <- mapM compileExpr exprs
        return $ Construct pos symbol exprs'

    AST.AExpr typ expr -> do
        expr' <- compileExpr expr
        return $ AExpr typ expr'

    AST.Null pos -> return (Null pos)

    AST.Match pos expr pat -> do
        expr' <- compileExpr expr
        pat' <- compilePattern pat
        return $ Match pos expr' pat'

    AST.Range pos mexpr mexpr1 mexpr2 -> do
        mexpr' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr
        mexpr1' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr1
        mexpr2' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr2
        return $ AST.Range pos mexpr' mexpr1' mexpr2'
compileExpr expr = withPos expr $ fail "what"


compilePattern :: BoM ASTResolved m => AST.Pattern -> m Pattern
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

    AST.PatGuarded pos pat expr -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        return $ PatGuarded pos pat' expr'

    AST.PatArray pos pats -> PatArray pos <$> mapM compilePattern pats

    AST.PatAnnotated pat typ -> do
        pat' <- compilePattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos


