{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IRGen where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST
import IR
import Symbol
import Monad
import Error
import Type
import Interop


prettyIrGenState :: IRGenState -> IO ()
prettyIrGenState irGenState = do
    putStrLn $ "module: " ++ moduleName irGenState
    forM_ (Map.toList $ typeDefs irGenState) $ \(symbol, _) -> do
        putStrLn $ "type: " ++ show symbol

    forM_ (Map.toList $ externDefs irGenState) $ \(name, (pts, sym, ats, rt)) -> do
        putStrLn $ "extern: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt ++ " " ++ show name

    when (isJust $ mainDef irGenState) $ do
        putStrLn $ "main: " ++ (show $ length $ funcStmts $ fromJust $ mainDef irGenState)

    forM_ (Map.toList $ funcDefs irGenState) $ \((pts, sym, ats, rt), body) -> do
        putStrLn $ "func: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt
        putStrLn $ "    params: " ++ AST.brcStrs (map show $ funcParams body)
        putStrLn $ "    args:   " ++ AST.tupStrs (map show $ funcArgs body)
        putStrLn $ "    name:   " ++ show (funcUniqueName body)
        putStrLn ""


type FuncKey = ([Type], String, [Type], Type)
data FuncBody = FuncBody
    { funcParams :: [AST.Param]
    , funcArgs   :: [AST.Param]
    , funcStmts  :: [Stmt]
    , funcUniqueName :: Symbol
    }


data StmtBlock = StmtBlock
    { stmts :: [Stmt]
    }


data IRGenState
    = IRGenState
        { imports :: [IRGenState]
        , cExterns :: [Extern]
        , moduleName :: String
        , typeDefs :: Map.Map Symbol AST.AnnoType
        , externDefs :: Map.Map Symbol FuncKey
        , funcDefs :: Map.Map FuncKey FuncBody
        , funcSymbolMap :: Map.Map Symbol FuncKey
        , mainDef  :: Maybe FuncBody
        , currentFunc :: FuncKey
        , blockStack :: [StmtBlock]
        }


initIRGenState moduleName imports cExterns = IRGenState
    { imports        = imports
    , cExterns       = cExterns
    , moduleName     = moduleName
    , typeDefs       = Map.empty
    , externDefs     = Map.empty
    , funcDefs       = Map.empty
    , funcSymbolMap  = Map.empty
    , mainDef        = Nothing
    , currentFunc    = ([], "", [], Void)
    , blockStack     = []
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
    forM_ funcDefStmts $ \(AST.FuncDef _ params symbol args retty _) ->
        case sym symbol of
            "main" -> do
                Nothing <- gets mainDef
                modify $ \s -> s { mainDef = Just (FuncBody [] [] [] (Sym "main")) }
                
            _ -> do
                let paramTypes = map AST.paramType params
                let argTypes   = map AST.paramType args
                let key        = (paramTypes, sym symbol, argTypes, retty)
                Nothing <- Map.lookup key <$> gets funcDefs
                modify $ \s -> s { funcDefs = Map.insert key (FuncBody [] [] [] symbol) (funcDefs s) }


initialiseTopTypeDefs :: BoM IRGenState m => AST.AST -> m ()
initialiseTopTypeDefs ast = do
    let typeDefStmts = [ x | x@(AST.Typedef _ _ _) <- AST.astStmts ast]
    forM_ typeDefStmts $ \(AST.Typedef _ symbol anno) -> do
        Nothing <- Map.lookup symbol <$> gets typeDefs
        modify $ \s -> s { typeDefs = Map.insert symbol anno (typeDefs s) }



exprTypeOf :: AST.Expr -> Type
exprTypeOf (AST.AExpr typ _) = typ



-- add extern if needed
resolveFuncCall :: BoM IRGenState m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = do
    let paramTypes = map exprTypeOf params
    let argTypes = map exprTypeOf args
    curModName <- gets moduleName
    imports <- gets imports
    case symbol of
        SymResolved _ _ _ -> return symbol
        SymQualified "c" sym -> do
            resm <- findCExtern sym
            assert (isJust resm) $ "no c extern definition for: " ++ sym
            let (ats, rt) = fromJust resm
            assert (paramTypes == [] && ats == argTypes && rt == exprType) "c extern mismatch"
            let key = ([], sym, ats, rt)
            let symbol = SymQualified "c" sym
            modify $ \s -> s { externDefs = Map.insert symbol key (externDefs s) }
            return symbol

        SymQualified mod sym | mod == curModName -> do
            let key = (paramTypes, sym, argTypes, exprType)
            resm <- findLocalFuncDef key
            assert (isJust resm) $ "no definition for: " ++ show key
            return $ fromJust resm

        SymQualified mod sym -> do
            let key = (paramTypes, sym, argTypes, exprType)
            resm <- findQualifiedImportedFuncDef mod key
            assert (isJust resm) $ "no definition for: " ++ show key
            let symbol = fromJust resm
            modify $ \s -> s { externDefs = Map.insert symbol key (externDefs s) }
            return symbol

        Sym sym -> do
            let key = (paramTypes, sym, argTypes, exprType)
            resm <- findLocalFuncDef key
            case resm of
                Just symbol -> return symbol
                Nothing -> do
                    resultm <- findImportedFuncDef key
                    assert (isJust resultm) $ "no definition for: " ++ show key
                    let symbol = fromJust resultm
                    modify $ \s -> s { externDefs = Map.insert symbol key (externDefs s) }
                    return symbol
    where
        findLocalFuncDef :: BoM IRGenState m => FuncKey -> m (Maybe Symbol)
        findLocalFuncDef key = do
            fmap funcUniqueName . Map.lookup key <$> gets funcDefs

        findImportedFuncDef :: BoM IRGenState m => FuncKey -> m (Maybe Symbol)
        findImportedFuncDef key = do
            imports <- gets imports
            case catMaybes $ map (Map.lookup key . funcDefs) imports of
                [] -> return Nothing 
                [x] -> return $ Just (funcUniqueName x)
                _    -> fail $ "multiple definitions for: " ++ show key
        
        findQualifiedImportedFuncDef :: BoM IRGenState m => String -> FuncKey -> m (Maybe Symbol)
        findQualifiedImportedFuncDef mod key = do
            imports <- gets imports
            let impm = find ((mod ==) . moduleName) imports
            assert (isJust impm) $ mod ++ " isn't imported"
            return $ fmap funcUniqueName $ Map.lookup key (funcDefs $ fromJust impm)

        findCExtern :: BoM IRGenState m => String -> m (Maybe ([Type], Type))
        findCExtern sym = do
            externs <- gets cExterns
            ress <- fmap catMaybes $ forM externs $ \ext -> case ext of
                ExtFunc s ats rt | s == sym -> return (Just (ats, rt))
                _                           -> return Nothing
            case ress of
                [] -> return Nothing
                [x] -> return (Just x)
                _   -> fail $ "multiple c extern definitions for: " ++ sym
                



compileStmt :: BoM IRGenState m => AST.Stmt -> m Stmt
compileStmt stmt = case stmt of
    AST.FuncDef pos params symbol args retty blk -> do
        let paramTypes = map AST.paramType params
        let argTypes   = map AST.paramType args
        let key        = (paramTypes, sym symbol, argTypes, retty)

        oldCurrentFunc <- gets currentFunc
        modify $ \s -> s { currentFunc = key }

        blk' <- compileStmt blk

        modify $ \s -> s { funcSymbolMap = Map.insert symbol key (funcSymbolMap s) }
        let funcBody = FuncBody {
            funcParams = params,
            funcArgs   = args,
            funcStmts  = [blk'],
            funcUniqueName = symbol
            }

        case sym symbol of 
            "main" -> modify $ \s -> s { mainDef = Just funcBody }
            _      -> modify $ \s -> s { funcDefs = Map.insert key funcBody (funcDefs s) }
        modify $ \s -> s { currentFunc = oldCurrentFunc }
        return $ FuncDef pos params symbol args retty blk'


    AST.ExprStmt expr -> do
        expr' <- compileExpr expr
        return $ ExprStmt expr'

    AST.Block stmts -> do
        stmts' <- mapM compileStmt stmts
        return $ Block stmts'

    AST.Return pos mexpr -> do
        mexpr' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr
        return $ Return pos mexpr'

    AST.Assign pos pat expr -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        return $ Assign pos pat' expr'
    
    AST.Typedef pos symbol anno -> do
        return $ IR.Typedef pos symbol anno

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

    AST.Data pos symbol typ -> do
        return $ Data pos symbol typ



compileExpr :: BoM IRGenState m => AST.Expr -> m Expr
compileExpr (AST.AExpr exprType expr) = AExpr exprType <$> case expr of
    AST.Ident pos symbol   -> return $ Ident pos symbol
    AST.Prefix pos op expr -> Prefix pos op <$> compileExpr expr
    AST.Char pos c         -> return $ IR.Char pos c
    AST.Len pos expr       -> Len pos <$> compileExpr expr
    AST.UnsafePtr pos expr -> IR.UnsafePtr pos <$> compileExpr expr
    AST.Int pos n          -> return $ Int pos n
    AST.Bool pos b         -> return $ IR.Bool pos b
    AST.Float pos f        -> return $ Float pos f
    AST.Tuple pos exprs    -> IR.Tuple pos <$> mapM compileExpr exprs
    AST.Array pos exprs    -> IR.Array pos <$> mapM compileExpr exprs
    AST.String pos s       -> return $ IR.String pos s

    AST.Push pos expr exprs -> do
        expr' <- compileExpr expr
        exprs' <- mapM compileExpr exprs
        return $ Push pos expr' exprs'

    AST.Pop pos expr exprs -> do
        expr' <- compileExpr expr
        exprs' <- mapM compileExpr exprs
        return $ Pop pos expr' exprs'

    AST.Clear pos expr -> do
        expr' <- compileExpr expr
        return $ Clear pos expr'

    AST.Delete pos expr1 expr2 -> do
        expr1' <- compileExpr expr1
        expr2' <- compileExpr expr2
        return $ Delete pos expr1' expr2'

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

    AST.Field pos expr sym -> do
        expr' <- compileExpr expr
        return $ Field pos expr' sym

    AST.AExpr typ expr -> do
        expr' <- compileExpr expr
        return $ AExpr typ expr'

    AST.TupleIndex pos expr i -> do
        expr' <- compileExpr expr
        return $ TupleIndex pos expr' i

    AST.Null pos -> return (Null pos)

    AST.ADT pos expr -> IR.ADT pos <$> compileExpr expr

    AST.Match pos expr pat -> do
        expr' <- compileExpr expr
        pat' <- compilePattern pat
        return $ Match pos expr' pat'

    AST.Range pos mexpr mexpr1 mexpr2 -> do
        mexpr' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr
        mexpr1' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr1
        mexpr2' <- maybe (return Nothing) (fmap Just . compileExpr) mexpr2
        return $ IR.Range pos mexpr' mexpr1' mexpr2'



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

    AST.PatGuarded pos pat expr -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        return $ PatGuarded pos pat' expr'

    AST.PatArray pos pats -> PatArray pos <$> mapM compilePattern pats

    AST.PatAnnotated pat typ -> do
        pat' <- compilePattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos


convertToIrStmt :: Monad m => AST.Stmt -> m Stmt
convertToIrStmt stmt = case stmt of
    AST.FuncDef pos params symbol args retty blk -> do
        blk' <- convertToIrStmt blk
        return $ FuncDef pos params symbol args retty blk'

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

