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

    forM_ (Map.toList $ irExternDefs irGenState) $ \(name, (pts, sym, ats, rt)) -> do
        putStrLn $ "extern: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt ++ " " ++ show name

    when (isJust $ irMainDef irGenState) $ do
        putStrLn $ "main: " ++ (show $ length $ funcStmts $ fromJust $ irMainDef irGenState)

    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, body) -> do
        putStrLn $ "func: " ++ show symbol
        putStrLn $ "    params: " ++ AST.brcStrs (map show $ funcParams body)
        putStrLn $ "    args:   " ++ AST.tupStrs (map show $ funcArgs body)
        putStrLn ""


initIRGenState moduleName imports cExterns = IRGenState
    { irImports        = imports
    , irCExterns       = cExterns
    , irModuleName     = moduleName
    , irTypeDefs       = Map.empty
    , irExternDefs     = Map.empty
    , irFuncDefs       = Map.empty
    , irFuncMap        = Map.empty
    , irTypeMap        = Map.empty
    , irMainDef        = Nothing
    , irCurrentFunc    = ([], "", [], Void)
    }


compile :: BoM IRGenState m => ResolvedAst -> m ()
compile ast = do
    initialiseTypeImports
    initialiseTopTypeDefs (typeDefs ast)
    initialiseTopFuncDefs (funcDefs ast)
    forM_ (typeDefs ast ++ funcDefs ast) $ \stmt -> compileStmt stmt



initialiseTypeImports :: BoM IRGenState m => m ()
initialiseTypeImports = do
    imports <- gets irImports
    forM_ imports $ \imprt -> do
        forM_ (Map.elems $ irTypeMap imprt) $ \symbol -> do
            let anno = irTypeDefs imprt Map.! symbol
            isDefined <- Map.member symbol <$> gets irTypeDefs
            assert (not isDefined) "type already defined"
            modify $ \s -> s { irTypeDefs = Map.insert symbol anno (irTypeDefs s) }
            


initialiseTopFuncDefs :: BoM IRGenState m => [AST.Stmt] -> m ()
initialiseTopFuncDefs stmts = do
    forM_ stmts $ \(AST.FuncDef _ params symbol args retty _) ->
        case sym symbol of
            "main" -> do
                Nothing <- gets irMainDef
                modify $ \s -> s { irMainDef = Just (FuncBody [] [] retty []) }
            _ -> do
                Nothing <- Map.lookup symbol <$> gets irFuncDefs
                let key = (map AST.paramType params, sym symbol, map AST.paramType args, retty)
                modify $ \s -> s { irFuncMap = Map.insert  key symbol (irFuncMap s) }
                modify $ \s -> s { irFuncDefs = Map.insert symbol (FuncBody params args retty []) (irFuncDefs s) }


initialiseTopTypeDefs :: BoM IRGenState m => [AST.Stmt] -> m ()
initialiseTopTypeDefs stmts = do
    forM_ stmts $ \(AST.Typedef _ symbol anno) -> do
        Nothing <- Map.lookup symbol <$> gets irTypeDefs
        Nothing <- Map.lookup (sym symbol) <$> gets irTypeMap
        modify $ \s -> s { irTypeDefs = Map.insert symbol anno (irTypeDefs s) }
        modify $ \s -> s { irTypeMap  = Map.insert (sym symbol) symbol (irTypeMap s) }



exprTypeOf :: AST.Expr -> Type
exprTypeOf (AST.AExpr typ _) = typ



-- add extern if needed
resolveFuncCall :: BoM IRGenState m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = withPos pos $ do
    let paramTypes = map exprTypeOf params
    let argTypes = map exprTypeOf args
    curModName <- gets irModuleName
    imports <- gets irImports
    case symbol of
        SymResolved _ _ _ -> return symbol
        SymQualified "c" sym -> do
            resm <- findCExtern sym
            assert (isJust resm) $ "no c extern definition for: " ++ sym
            let (ats, rt) = fromJust resm
            assert (paramTypes == [] && ats == argTypes && rt == exprType) $
                "c extern mismatch: " ++ show (paramTypes, argTypes, exprType, ats, rt)
            let key = ([], sym, ats, rt)
            let symbol = SymQualified "c" sym
            modify $ \s -> s { irExternDefs = Map.insert symbol key (irExternDefs s) }
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
            modify $ \s -> s { irExternDefs = Map.insert symbol key (irExternDefs s) }
            return symbol

        Sym sym -> do
            let key = (paramTypes, sym, argTypes, exprType)
            funcresm <- findLocalFuncDef key
            typeresm <- findLocalTypeDef sym
            case (funcresm, typeresm) of
                (Just x, Nothing) -> return x
                (Nothing, Just x) -> return x
                (Nothing, Nothing) -> do
                    funcresm <- findImportedFuncDef key
                    typeresm <- findImportedTypeDef sym
                    case (funcresm, typeresm) of
                        (Just x, Nothing) -> do
                            modify $ \s -> s { irExternDefs = Map.insert x key (irExternDefs s) }
                            return x
                        (Nothing, Just x) -> return x
                        (Nothing, Nothing) -> fail "no def"
    where
        findLocalTypeDef :: BoM IRGenState m => String -> m (Maybe Symbol)
        findLocalTypeDef sym = Map.lookup sym <$> gets irTypeMap

        findLocalFuncDef :: BoM IRGenState m => FuncKey -> m (Maybe Symbol)
        findLocalFuncDef key@(pts, sym, ats, rt) = Map.lookup key <$> gets irFuncMap

        findImportedFuncDef :: BoM IRGenState m => FuncKey -> m (Maybe Symbol)
        findImportedFuncDef key@(pts, sym, ats, rt) = do
            imports <- gets irImports
            let funcress = catMaybes $ map (Map.lookup key . irFuncMap) imports
            case funcress of
                [x] -> return (Just x)
                []  -> return Nothing
                _   -> fail $ "ambiguous function call: " ++ sym

        findImportedTypeDef :: BoM IRGenState m => String -> m (Maybe Symbol)
        findImportedTypeDef sym = do
            imports <- gets irImports
            let typeress = catMaybes $ map (Map.lookup sym . irTypeMap) imports
            case typeress of
                [x] -> return (Just x)
                []  -> return Nothing
                _   -> fail $ "ambiguous type: " ++ sym
        
        findQualifiedImportedFuncDef :: BoM IRGenState m => String -> FuncKey -> m (Maybe Symbol)
        findQualifiedImportedFuncDef mod key = do
            imports <- gets irImports
            let importm = find ((mod ==) . irModuleName) imports
            assert (isJust importm) $ mod ++ " isn't imported"
            return $ Map.lookup key (irFuncMap $ fromJust importm)

        findCExtern :: BoM IRGenState m => String -> m (Maybe ([Type], Type))
        findCExtern sym = do
            externs <- gets irCExterns
            ress <- fmap catMaybes $ forM externs $ \ext -> case ext of
                ExtFunc s ats rt | s == sym -> return (Just (ats, rt))
                _                           -> return Nothing
            case ress of
                [] -> return Nothing
                [x] -> return (Just x)
                _   -> fail $ "multiple c extern definitions for: " ++ sym
                


compileStmt :: BoM IRGenState m => AST.Stmt -> m Stmt
compileStmt stmt = withPos stmt $ case stmt of
    AST.FuncDef pos params symbol args retty blk -> do
        let paramTypes = map AST.paramType params
        let argTypes   = map AST.paramType args
        let key        = (paramTypes, sym symbol, argTypes, retty)

        oldCurrentFunc <- gets irCurrentFunc
        modify $ \s -> s { irCurrentFunc = key }

        blk' <- compileStmt blk

        let funcBody = FuncBody {
            funcParams = params,
            funcArgs   = args,
            funcRetty  = retty,
            funcStmts  = [blk']
            }

        case sym symbol of 
            "main" -> modify $ \s -> s { irMainDef = Just funcBody }
            _      -> modify $ \s -> s { irFuncDefs = Map.insert symbol funcBody (irFuncDefs s) }
        modify $ \s -> s { irCurrentFunc = oldCurrentFunc }
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
        return $ AST.Typedef pos symbol anno

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
compileExpr (AST.AExpr exprType expr) = withPos expr $ AExpr exprType <$> case expr of
    AST.Ident pos symbol   -> return $ Ident pos symbol
    AST.Prefix pos op expr -> Prefix pos op <$> compileExpr expr
    AST.Char pos c         -> return $ AST.Char pos c
    AST.Len pos expr       -> Len pos <$> compileExpr expr
    AST.UnsafePtr pos expr -> AST.UnsafePtr pos <$> compileExpr expr
    AST.Int pos n          -> return $ Int pos n
    AST.Bool pos b         -> return $ AST.Bool pos b
    AST.Float pos f        -> return $ Float pos f
    AST.Tuple pos exprs    -> AST.Tuple pos <$> mapM compileExpr exprs
    AST.Array pos exprs    -> AST.Array pos <$> mapM compileExpr exprs
    AST.String pos s       -> return $ AST.String pos s

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

    AST.PatGuarded pos pat expr -> do
        pat' <- compilePattern pat
        expr' <- compileExpr expr
        return $ PatGuarded pos pat' expr'

    AST.PatArray pos pats -> PatArray pos <$> mapM compilePattern pats

    AST.PatAnnotated pat typ -> do
        pat' <- compilePattern pat
        return $ PatAnnotated pat' typ

    AST.PatNull pos -> return $ PatNull pos


