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
import FunctionFinder


-- Resolves function calls
-- Creates generic instantiations
-- Resolves tuple/table field symbols
-- Turns ctor function call into Contructors


compile :: BoM ASTResolved m => m ()
compile = do
    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcTypeArgs body == []) $ do
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


-- add extern if needed
resolveFuncCall :: BoM ASTResolved m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = withPos pos $ do
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- get

    symbols <- case symbol of
        SymResolved _ _ _ -> return [symbol] -- already resolved
        _                 -> findCandidates callHeader ast

    exacts <- findExactFunction callHeader ast

    if exacts /= [] then do
        assert (length exacts == 1) "cannot have multiple exact matches"
        return $ head exacts
    else do -- TODO this part could be neater
        case symbols of
            [] -> do
                liftIO $ do
                    prettyASTResolved ast
                error $ "no candidates for:" ++ show callHeader
            [symbol'] -> do
                if (isGenericFunction symbol' ast) then do
                    let header  = getFunctionHeader symbol' ast
                    let body    = getFunctionBody symbol' ast
                    let sym     = Symbol.sym (ASTResolved.symbol header)
                    headerReplaced <- replaceGenericsInFuncHeaderWithCall header callHeader

                    if isJust headerReplaced && funcHeaderFullyResolved (fromJust headerReplaced) then do
                        exacts <- findExactFunction ((fromJust headerReplaced) { symbol = Symbol.Sym sym }) ast 
                        case exacts of 
                            [] -> do -- define specific type case
                                symbol'' <- genSymbol sym
                                body' <- replaceGenericsInFuncBodyWithCall body callHeader
                                modify $ \s -> s { funcDefs = Map.insert symbol'' body' (funcDefs ast) }
                                return symbol''
                            [symbol''] -> return symbol''
                    else return symbol'
                else do
                    return symbol'
            _ -> do

                return symbol


compileStmt :: BoM ASTResolved m => AST.Stmt -> m Stmt
compileStmt stmt = withPos stmt $ case stmt of
    AST.Increment pos expr -> AST.Increment pos <$> compileExpr expr
    AST.EmbedC pos s     -> return (AST.EmbedC pos s)
    AST.Block stmts      -> Block <$> mapM compileStmt stmts
    AST.ExprStmt expr    -> ExprStmt <$> compileExpr expr
    AST.Return pos mexpr -> Return pos <$> maybe (return Nothing) (fmap Just . compileExpr) mexpr
    AST.FuncDef pos typeArgs params symbol args retty blk -> return stmt
    AST.Const pos symbol expr -> return stmt
    AST.Typedef pos args symbol anno -> return $ AST.Typedef pos args symbol anno

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
        res <- fmap catMaybes $ forM (Map.toList ctors) $ \(symbol, (typeSymbol, i)) -> do
            exprTypeSymbolm <- case typeof expr of
--                Type.TypeApply s _ -> return (Just s)
                _ -> return Nothing
            case exprTypeSymbolm of
                Nothing -> return Nothing
                Just exprTypeSymbol -> do
                    if Symbol.sym symbol == sym && exprTypeSymbol == typeSymbol then
                        return $ Just symbol
                    else return Nothing

        case res of
            [] -> do
                expr' <- compileExpr expr
                return $ Field pos expr' (Sym sym)
            (a:b:xs)      -> fail "ambiguous"
            [symbol] -> do
                expr' <- compileExpr expr
                return $ Field pos expr' symbol


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

    AST.Subscript pos expr1 mexpr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr2
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


