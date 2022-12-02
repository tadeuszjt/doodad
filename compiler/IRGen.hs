{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IRGen where

import Data.Maybe
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
    forM_ (Map.toList $ typeDefs irGenState) $ \(symbol, _) -> do
        putStrLn $ "type: " ++ show symbol

    when (isJust $ mainDef irGenState) $ do
        putStrLn $ "main: " ++ (show $ length $ funcStmts $ fromJust $ mainDef irGenState)

    forM_ (Map.toList $ funcDefs irGenState) $ \((pts, sym, ats, rt), body) -> do
        putStrLn $ "func: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt
        putStrLn $ "    params: " ++ AST.brcStrs (map show $ funcParams body)
        putStrLn $ "    args:   " ++ AST.tupStrs (map show $ funcArgs body)
        putStrLn ""




type FuncKey = ([Type], String, [Type], Type)
data FuncBody = FuncBody
    { funcParams :: [AST.Param]
    , funcArgs   :: [AST.Param]
    , funcStmts  :: [Stmt]
    }


data StmtBlock = StmtBlock
    { stmts :: [Stmt]
    }


data IRGenState
    = IRGenState
        { moduleName :: String
        , typeDefs :: Map.Map Symbol AST.AnnoType
        , funcDefs :: Map.Map FuncKey FuncBody
        , mainDef  :: Maybe FuncBody
        , currentFunc :: FuncKey
        , blockStack :: [StmtBlock]
        }


initIRGenState moduleName = IRGenState
    { moduleName = moduleName
    , typeDefs = Map.empty
    , funcDefs = Map.empty
    , mainDef  = Nothing
    , currentFunc = ([], "", [], Void)
    , blockStack = []
    }


emitStmt :: BoM IRGenState m => Stmt -> m ()
emitStmt stmt = do
    stack <- gets blockStack
    let block = head stack
    modify $ \s -> s { blockStack = (block { stmts = stmts block ++ [stmt]}) : tail stack }



compile :: BoM IRGenState m => AST.AST -> m ()
compile ast = do
    initialiseTopTypeDefs ast
    initialiseTopFuncDefs ast
    forM_ (AST.astStmts ast) $ \stmt -> compileStmt stmt


initialiseTopFuncDefs :: BoM IRGenState m => AST.AST -> m ()
initialiseTopFuncDefs ast = do
    let funcDefStmts = [ x | x@(AST.FuncDef _ _ _ _ _ _) <- AST.astStmts ast]
    forM_ funcDefStmts $ \(AST.FuncDef _ params sym args retty _) ->
        case sym of
            "main" -> do
                Nothing <- gets mainDef
                modify $ \s -> s { mainDef = Just (FuncBody [] [] []) }
                
            _ -> do
                let paramTypes = map AST.paramType params
                let argTypes   = map AST.paramType args
                let key        = (paramTypes, sym, argTypes, retty)
                Nothing <- Map.lookup key <$> gets funcDefs
                modify $ \s -> s { funcDefs = Map.insert key (FuncBody [] [] []) (funcDefs s) }



initialiseTopTypeDefs :: BoM IRGenState m => AST.AST -> m ()
initialiseTopTypeDefs ast = do
    let typeDefStmts = [ x | x@(AST.Typedef _ _ _) <- AST.astStmts ast]
    forM_ typeDefStmts $ \(AST.Typedef _ symbol anno) -> do
        Nothing <- Map.lookup symbol <$> gets typeDefs
        modify $ \s -> s { typeDefs = Map.insert symbol anno (typeDefs s) }


compileStmt :: BoM IRGenState m => AST.Stmt -> m ()
compileStmt stmt = case stmt of
    AST.FuncDef pos params sym args retty blk -> do
        let paramTypes = map AST.paramType params
        let argTypes   = map AST.paramType args
        let key        = (paramTypes, sym, argTypes, retty)

        oldCurrentFunc <- gets currentFunc
        modify $ \s -> s { currentFunc = key }

        blk' <- convertToIrStmt blk
        let funcBody = FuncBody {
            funcParams = params,
            funcArgs   = args,
            funcStmts  = [blk']
            }

        case sym of 
            "main" -> modify $ \s -> s { mainDef = Just funcBody }
            _      -> modify $ \s -> s { funcDefs = Map.insert key funcBody (funcDefs s) }
        modify $ \s -> s { currentFunc = oldCurrentFunc }

        return ()

    _ -> return ()
    


convertToIrStmt :: Monad m => AST.Stmt -> m Stmt
convertToIrStmt stmt = case stmt of
    AST.FuncDef pos params sym args retty blk -> do
        blk' <- convertToIrStmt blk
        return $ FuncDef pos params sym args retty blk'

    AST.ExprStmt expr -> do
        expr' <- convertToIrExpr expr
        return $ ExprStmt expr'

    AST.Block stmts -> do
        stmts' <- mapM convertToIrStmt stmts
        return $ Block stmts'

    AST.Return pos mexpr -> do
        mexpr' <- maybe (return Nothing) (fmap Just . convertToIrExpr) mexpr
        return $ Return pos mexpr'

    AST.Assign pos pat expr -> do
        pat' <- convertToIrPattern pat
        expr' <- convertToIrExpr expr
        return $ Assign pos pat' expr'
    
    AST.Typedef pos symbol anno -> do
        return $ IR.Typedef pos symbol anno

    AST.If pos expr stmt melse -> do
        expr' <- convertToIrExpr expr
        stmt' <- convertToIrStmt stmt
        melse' <- maybe (return Nothing) (fmap Just . convertToIrStmt) melse
        return $ If pos expr' stmt' melse'

    AST.While pos expr stmt -> do
        expr' <- convertToIrExpr expr
        stmt' <- convertToIrStmt stmt
        return $ While pos expr' stmt'

    AST.Set pos expr1 expr2 -> do
        expr1' <- convertToIrExpr expr1
        expr2' <- convertToIrExpr expr2
        return $ Set pos expr1' expr2'

    AST.Print pos exprs -> Print pos <$> mapM convertToIrExpr exprs

    AST.Switch pos expr cases -> do
        expr' <- convertToIrExpr expr
        cases' <- forM cases $ \(pat, stmt) -> do
            pat' <- convertToIrPattern pat
            stmt' <- convertToIrStmt stmt
            return (pat', stmt')
        return $ Switch pos expr' cases'
    
    AST.For pos expr mpat blk -> do
        expr' <- convertToIrExpr expr
        mpat' <- maybe (return Nothing) (fmap Just . convertToIrPattern) mpat
        blk' <- convertToIrStmt blk
        return $ For pos expr' mpat' blk'

    AST.Data pos symbol typ -> do
        return $ Data pos symbol typ



convertToIrExpr :: Monad m => AST.Expr -> m Expr
convertToIrExpr expr = case expr of
    AST.Ident pos symbol   -> return $ Ident pos symbol
    AST.Prefix pos op expr -> Prefix pos op <$> convertToIrExpr expr
    AST.Char pos c         -> return $ IR.Char pos c
    AST.Len pos expr       -> Len pos <$> convertToIrExpr expr
    AST.UnsafePtr pos expr -> IR.UnsafePtr pos <$> convertToIrExpr expr
    AST.Int pos n          -> return $ Int pos n
    AST.Bool pos b         -> return $ IR.Bool pos b
    AST.Float pos f        -> return $ Float pos f
    AST.Tuple pos exprs    -> IR.Tuple pos <$> mapM convertToIrExpr exprs
    AST.Array pos exprs    -> IR.Array pos <$> mapM convertToIrExpr exprs
    AST.String pos s       -> return $ IR.String pos s

    AST.Push pos expr exprs -> do
        expr' <- convertToIrExpr expr
        exprs' <- mapM convertToIrExpr exprs
        return $ Push pos expr' exprs'

    AST.Pop pos expr exprs -> do
        expr' <- convertToIrExpr expr
        exprs' <- mapM convertToIrExpr exprs
        return $ Pop pos expr' exprs'

    AST.Clear pos expr -> do
        expr' <- convertToIrExpr expr
        return $ Clear pos expr'

    AST.Delete pos expr1 expr2 -> do
        expr1' <- convertToIrExpr expr1
        expr2' <- convertToIrExpr expr2
        return $ Delete pos expr1' expr2'

    AST.Call pos params symbol exprs -> do
        params' <- mapM convertToIrExpr params
        exprs' <- mapM convertToIrExpr exprs
        return $ Call pos params' symbol exprs'

    AST.Infix pos op expr1 expr2 -> do
        expr1' <- convertToIrExpr expr1
        expr2' <- convertToIrExpr expr2
        return $ Infix pos op expr1' expr2'

    AST.Subscript pos expr1 expr2 -> do
        expr1' <- convertToIrExpr expr1
        expr2' <- convertToIrExpr expr2
        return $ Subscript pos expr1' expr2'

    AST.Conv pos typ exprs -> do
        exprs' <- mapM convertToIrExpr exprs
        return $ Conv pos typ exprs'

    AST.Field pos expr sym -> do
        expr' <- convertToIrExpr expr
        return $ Field pos expr' sym

    AST.AExpr typ expr -> do
        expr' <- convertToIrExpr expr
        return $ AExpr typ expr'

    AST.TupleIndex pos expr i -> do
        expr' <- convertToIrExpr expr
        return $ TupleIndex pos expr' i

    AST.Null pos -> return (Null pos)

    AST.ADT pos expr -> IR.ADT pos <$> convertToIrExpr expr

    AST.Match pos expr pat -> do
        expr' <- convertToIrExpr expr
        pat' <- convertToIrPattern pat
        return $ Match pos expr' pat'

    AST.Range pos mexpr mexpr1 mexpr2 -> do
        mexpr' <- maybe (return Nothing) (fmap Just . convertToIrExpr) mexpr
        mexpr1' <- maybe (return Nothing) (fmap Just . convertToIrExpr) mexpr1
        mexpr2' <- maybe (return Nothing) (fmap Just . convertToIrExpr) mexpr2
        return $ IR.Range pos mexpr' mexpr1' mexpr2'



convertToIrPattern :: Monad m => AST.Pattern -> m Pattern
convertToIrPattern pattern = case pattern of
    AST.PatIgnore pos -> return $ PatIgnore pos
    AST.PatIdent pos symbol -> do
        return $ PatIdent pos symbol

    AST.PatField pos symbol pats -> do
        pats' <- mapM convertToIrPattern pats
        return $ PatField pos symbol pats'

    AST.PatTypeField pos typ pat -> do
        pat' <- convertToIrPattern pat
        return $ PatTypeField pos typ pat'

    AST.PatTuple pos pats -> PatTuple pos <$> mapM convertToIrPattern pats

    AST.PatLiteral expr -> PatLiteral <$> convertToIrExpr expr

    AST.PatGuarded pos pat expr -> do
        pat' <- convertToIrPattern pat
        expr' <- convertToIrExpr expr
        return $ PatGuarded pos pat' expr'

    AST.PatArray pos pats -> PatArray pos <$> mapM convertToIrPattern pats

    AST.PatAnnotated pat typ -> do
        pat' <- convertToIrPattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos

