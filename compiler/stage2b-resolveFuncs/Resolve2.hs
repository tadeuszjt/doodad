{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Resolve2 where

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


compile :: BoM ASTResolved m => m ()
compile = do
    funcDefs <- gets funcDefs
    funcDefs' <- fmap Map.fromList $ forM (Map.toList funcDefs) $ \(symbol, body) -> do
        body' <- do
            stmt' <- compileStmt (funcStmt body)
            return body { funcStmt = stmt' }
        return (symbol, body')
    modify $ \s -> s { funcDefs = funcDefs' }




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
        findFuncDef :: BoM ASTResolved m => FuncKey -> m (Maybe Symbol)
        findFuncDef key = checkOne =<< Map.filterWithKey (\symbol body -> funcKeyFromBody (sym symbol) body == key) <$> gets funcDefs
--        findFuncDef :: BoM ASTResolved m => FuncKey -> m (Maybe Symbol)
--        findFuncDef key = checkOne =<< Map.filterWithKey (\symbol body -> funcKeysCouldMatch (funcKeyFromBody (sym symbol) body) key) <$> gets funcDefs
--            where
--                funcKeysCouldMatch :: FuncKey -> FuncKey -> Bool
--                funcKeysCouldMatch (aps, asymbol, aas, art) (bps, bsymbol, bas, brt)
--                    | length aps /= length bps || length aas /= length bas = False
--                    | otherwise = all (== True) $ zipWith typesCouldMatch (aps ++ aas ++ [art]) (bps ++ bas ++ [brt])


        findTypeDef :: BoM ASTResolved m => String -> m (Maybe Symbol)
        findTypeDef sym = checkOne =<< Map.filterWithKey (\s t -> Symbol.sym s == sym) <$> gets typeDefs

        findImportedFuncDef :: BoM ASTResolved m => FuncKey -> m (Maybe Symbol)
        findImportedFuncDef key = checkOne =<< Map.filter (== key) <$> gets funcImports

        findImportedTypeDef :: BoM ASTResolved m => String -> m (Maybe Symbol)
        findImportedTypeDef sym = checkOne =<< Map.filterWithKey (\s t -> Symbol.sym s == sym) <$> gets typeImports

        findQualifiedFuncDef :: BoM ASTResolved m => String -> FuncKey -> m (Maybe Symbol)
        findQualifiedFuncDef mod key = checkOne =<< Map.filterWithKey (\s k -> Symbol.mod s == mod && k == key) <$> gets funcImports

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
    AST.Typedef pos symbol anno -> return $ AST.Typedef pos symbol anno
    AST.FuncDef pos generics params symbol args retty blk -> return stmt
    AST.Const pos symbol expr -> return stmt

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
        ctorDefs <- gets ctorDefs
        ctorImports <- gets ctorImports
        let ctors = Map.union ctorDefs ctorImports
        let res = Map.toList $ Map.filterWithKey (\k a -> tupTypeMatches a && fieldSymMatches k) (Map.union ctorDefs ctorImports)
        case res of
            [] -> fail "No ctor found"
            (a:b:xs) -> fail "ambiguous"
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
    AST.Field pos _ _         -> resolveFieldAccess expr
    AST.Ident pos symbol      -> return $ Ident pos symbol
    AST.Prefix pos op expr    -> Prefix pos op <$> compileExpr expr
    AST.Char pos c            -> return $ AST.Char pos c
    AST.Int pos n             -> return $ Int pos n
    AST.Bool pos b            -> return $ AST.Bool pos b
    AST.Float pos f           -> return $ Float pos f
    AST.Tuple pos exprs       -> AST.Tuple pos <$> mapM compileExpr exprs
    AST.String pos s          -> return $ AST.String pos s
    AST.Array pos exprs       -> AST.Array pos <$> mapM compileExpr exprs

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

        isLocalCtor <- Map.member symbol' <$> gets ctorDefs
        isImportedCtor <- Map.member symbol' <$> gets ctorImports
        if isLocalCtor || isImportedCtor then do
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

    AST.PatGuarded pos pat expr mpat -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        mpat' <- maybe (return Nothing) (fmap Just . compilePattern) mpat
        return $ PatGuarded pos pat' expr' mpat'

    AST.PatArray pos pats -> PatArray pos <$> mapM compilePattern pats

    AST.PatAnnotated pat typ -> do
        pat' <- compilePattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos


