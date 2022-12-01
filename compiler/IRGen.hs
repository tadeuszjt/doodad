{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IRGen where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import qualified AST
import IR
import Symbol
import Monad
import Error
import Type


prettyIrGenState :: IRGenState -> IO ()
prettyIrGenState irGenState = do
    putStrLn $ "module: " ++ moduleName irGenState
    forM_ (Map.toList $ funcDefs irGenState) $ \((pts, sym, ats, rt), stmts) -> do
        putStrLn $ "func: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt
        forM_ stmts $ \stmt -> do
            prettyStmt "\t" stmt



class IRGen a b where
    irGen :: BoM IRGenState m => a -> m b



type FuncKey = ([Type], String, [Type], Type)


data IRGenState
    = IRGenState
        { moduleName :: String
        , funcDefs :: Map.Map FuncKey [Stmt]
        , currentFunc :: FuncKey
        }


initIRGenState moduleName = IRGenState
    { moduleName = moduleName
    , funcDefs = Map.empty
    , currentFunc = ([], "", [], Void)
    }


instance IRGen AST.AST IR where
    irGen ast = do
        initialiseTopFuncDefs ast

        irStmts <- mapM irGen $ AST.astStmts ast
        return $ IR
            { irStmts = irStmts
            , irModuleName = AST.astModuleName ast
            , irImports = AST.astImports ast
            }


initialiseTopFuncDefs :: BoM IRGenState m => AST.AST -> m ()
initialiseTopFuncDefs ast = do
    let funcDefStmts = [ x | x@(AST.FuncDef _ _ _ _ _ _) <- AST.astStmts ast]
    forM_ funcDefStmts $ \(AST.FuncDef _ params sym args retty _) -> do
        let paramTypes = map AST.paramType params
        let argTypes   = map AST.paramType args
        let key        = (paramTypes, sym, argTypes, retty)
        Nothing <- Map.lookup key <$> gets funcDefs
        modify $ \s -> s { funcDefs = Map.insert key [] (funcDefs s) }

    let mainKey = ([], "main.global", [], Void)
    modify $ \s -> s { currentFunc = mainKey }
    modify $ \s -> s { funcDefs = Map.insert mainKey [] (funcDefs s) }
        

emitStmt :: BoM IRGenState m => Stmt -> m ()
emitStmt stmt = do
    currentFunc <- gets currentFunc
    resm <- Map.lookup currentFunc <$> gets funcDefs
    case resm of
        Nothing -> fail $ "Could not find: " ++ show currentFunc
        Just res -> modify $ \s -> s { funcDefs = Map.insert currentFunc (res ++ [stmt]) (funcDefs s) }



instance IRGen AST.Stmt Stmt where
    irGen stmt = case stmt of
        AST.FuncDef pos params sym args retty blk -> do
            let paramTypes = map AST.paramType params
            let argTypes   = map AST.paramType args
            let key        = (paramTypes, sym, argTypes, retty)

            oldCurrentFunc <- gets currentFunc
            modify $ \s -> s { currentFunc = key }
            blk' <- irGen blk
            modify $ \s -> s { currentFunc = oldCurrentFunc }
            return $ FuncDef pos params sym args retty blk'

        AST.ExprStmt expr -> do
            expr' <- irGen expr
            return $ ExprStmt expr'

        AST.Block stmts -> do
            stmts' <- mapM irGen stmts
            return $ Block stmts'

        AST.Return pos mexpr -> do
            mexpr' <- maybe (return Nothing) (fmap Just . irGen) mexpr
            return $ Return pos mexpr'

        AST.Assign pos pat expr -> do
            pat' <- irGen pat
            expr' <- irGen expr
            return $ Assign pos pat' expr'
        
        AST.Typedef pos symbol anno -> do
            return $ IR.Typedef pos symbol anno

        AST.If pos expr stmt melse -> do
            expr' <- irGen expr
            stmt' <- irGen stmt
            melse' <- maybe (return Nothing) (fmap Just . irGen) melse
            return $ If pos expr' stmt' melse'

        AST.While pos expr stmt -> do
            expr' <- irGen expr
            stmt' <- irGen stmt
            return $ While pos expr' stmt'

        AST.Set pos expr1 expr2 -> do
            expr1' <- irGen expr1
            expr2' <- irGen expr2
            return $ Set pos expr1' expr2'

        AST.Print pos exprs -> Print pos <$> mapM irGen exprs

        AST.Switch pos expr cases -> do
            expr' <- irGen expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- irGen pat
                stmt' <- irGen stmt
                return (pat', stmt')
            return $ Switch pos expr' cases'
        
        AST.For pos expr mpat blk -> do
            expr' <- irGen expr
            mpat' <- maybe (return Nothing) (fmap Just . irGen) mpat
            blk' <- irGen blk
            return $ For pos expr' mpat' blk'

        AST.Data pos symbol typ -> do
            return $ Data pos symbol typ

--        _ -> return stmt
        _ -> fail $ show stmt



instance IRGen AST.Expr Expr where
    irGen expr = case expr of
        AST.Ident pos symbol   -> return $ Ident pos symbol
        AST.Prefix pos op expr -> Prefix pos op <$> irGen expr
        AST.Char pos c         -> return $ IR.Char pos c
        AST.Len pos expr       -> Len pos <$> irGen expr
        AST.UnsafePtr pos expr -> IR.UnsafePtr pos <$> irGen expr
        AST.Int pos n          -> return $ Int pos n
        AST.Bool pos b         -> return $ IR.Bool pos b
        AST.Float pos f        -> return $ Float pos f
        AST.Tuple pos exprs    -> IR.Tuple pos <$> mapM irGen exprs
        AST.Array pos exprs    -> IR.Array pos <$> mapM irGen exprs
        AST.String pos s       -> return $ IR.String pos s

        AST.Push pos expr exprs -> do
            expr' <- irGen expr
            exprs' <- mapM irGen exprs
            return $ Push pos expr' exprs'

        AST.Pop pos expr exprs -> do
            expr' <- irGen expr
            exprs' <- mapM irGen exprs
            return $ Pop pos expr' exprs'

        AST.Clear pos expr -> do
            expr' <- irGen expr
            return $ Clear pos expr'

        AST.Delete pos expr1 expr2 -> do
            expr1' <- irGen expr1
            expr2' <- irGen expr2
            return $ Delete pos expr1' expr2'

        AST.Call pos params symbol exprs -> do
            params' <- mapM irGen params
            exprs' <- mapM irGen exprs
            return $ Call pos params' symbol exprs'

        AST.Infix pos op expr1 expr2 -> do
            expr1' <- irGen expr1
            expr2' <- irGen expr2
            return $ Infix pos op expr1' expr2'

        AST.Subscript pos expr1 expr2 -> do
            expr1' <- irGen expr1
            expr2' <- irGen expr2
            return $ Subscript pos expr1' expr2'

        AST.Conv pos typ exprs -> do
            exprs' <- mapM irGen exprs
            return $ Conv pos typ exprs'

        AST.Field pos expr sym -> do
            expr' <- irGen expr
            return $ Field pos expr' sym

        AST.AExpr typ expr -> do
            expr' <- irGen expr
            return $ AExpr typ expr'

        AST.TupleIndex pos expr i -> do
            expr' <- irGen expr
            return $ TupleIndex pos expr' i

        AST.Null pos -> return (Null pos)

        AST.ADT pos expr -> IR.ADT pos <$> irGen expr

        AST.Match pos expr pat -> do
            expr' <- irGen expr
            pat' <- irGen pat
            return $ Match pos expr' pat'

        AST.Range pos mexpr mexpr1 mexpr2 -> do
            mexpr' <- maybe (return Nothing) (fmap Just . irGen) mexpr
            mexpr1' <- maybe (return Nothing) (fmap Just . irGen) mexpr1
            mexpr2' <- maybe (return Nothing) (fmap Just . irGen) mexpr2
            return $ IR.Range pos mexpr' mexpr1' mexpr2'

        --_ -> return expr

        _ -> fail $ "invalid expression: " ++ show expr


instance IRGen AST.Pattern Pattern where
    irGen pattern = case pattern of
        AST.PatIgnore pos -> return $ PatIgnore pos
        AST.PatIdent pos symbol -> do
            return $ PatIdent pos symbol

        AST.PatField pos symbol pats -> do
            pats' <- mapM irGen pats
            return $ PatField pos symbol pats'

        AST.PatTypeField pos typ pat -> do
            pat' <- irGen pat
            return $ PatTypeField pos typ pat'

        AST.PatTuple pos pats -> PatTuple pos <$> mapM irGen pats

        AST.PatLiteral expr -> PatLiteral <$> irGen expr

        AST.PatGuarded pos pat expr -> do
            pat' <- irGen pat
            expr' <- irGen expr
            return $ PatGuarded pos pat' expr'

        AST.PatArray pos pats -> PatArray pos <$> mapM irGen pats

        AST.PatAnnotated pat typ -> do
            pat' <- irGen pat
            return $ PatAnnotated pat' typ

        AST.PatNull pos -> return $ PatNull pos

        _ -> error $ "invalid pattern: " ++ show pattern

