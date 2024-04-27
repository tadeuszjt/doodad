module FunctionInstantiator where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import AST
import ASTResolved
import ASTMapper
import Symbol
import Monad
import Error
import Type
import Apply
import FunctionFinder
import qualified SymTab

type SymTab = SymTab.SymTab Symbol () FuncHeader

data InstantiatorState
    = InstantiatorState
        { astResolved :: ASTResolved
        , symTab      :: SymTab
        }

initInstantiatorState ast = InstantiatorState
    { astResolved = ast
    , symTab      = SymTab.initSymTab
    }


pushSymTab :: DoM InstantiatorState ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }

popSymTab :: DoM InstantiatorState ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


modifyAST :: (ASTResolved -> ASTResolved) -> DoM InstantiatorState ()
modifyAST f = modify $ \s -> s { astResolved = f (astResolved s) }


compile :: Bool -> ASTResolved -> DoM s ASTResolved
compile verbose ast = do
    x <- fmap snd $ runDoMExcept (initInstantiatorState ast) $ do
        instAst verbose
    return (astResolved x)

-- Resolves function calls
-- Creates generic instantiations
instAst :: Bool -> DoM InstantiatorState ()
instAst verbose = do
    --when verbose $ liftIO $ putStrLn $ "cleaning..."
    funcInstances <- gets (funcInstances . astResolved)
    forM_ (Map.toList funcInstances) $ \(symbol, func) -> do
        when (funcGenerics (funcHeader func) == []) $ do
            pushSymTab
            stmt' <- instStmt (funcStmt func)
            func' <- return func { funcStmt = stmt' }
            modifyAST $ \s -> s { funcInstances = Map.insert symbol func' (ASTResolved.funcInstances s) }
            popSymTab

    funcDefs <- gets (funcDefs . astResolved)
    forM_ (Map.toList funcDefs) $ \(symbol, func) -> do
        when (funcGenerics (funcHeader func) == []) $ do
            pushSymTab
            stmt' <- instStmt (funcStmt func)
            func' <- return func { funcStmt = stmt' }
            modifyAST $ \s -> s { funcDefs = Map.insert symbol func' (ASTResolved.funcDefs s) }
            popSymTab



instExpr :: Expr -> DoM InstantiatorState Expr
instExpr = mapExprM instantiatorMapper

instPattern :: Pattern -> DoM InstantiatorState Pattern
instPattern = mapPattern instantiatorMapper


instPatternIsolated :: Pattern -> DoM InstantiatorState Pattern
instPatternIsolated = mapPatternIsolated instantiatorMapper


instStmt :: Stmt -> DoM InstantiatorState Stmt
instStmt stmt = withPos stmt $ case stmt of
    Typedef _ _ _ _ -> return stmt
    EmbedC pos s -> return stmt
    ExprStmt expr -> ExprStmt <$> instExpr expr
    Return pos mexpr -> Return pos <$> traverse instExpr mexpr

    Block stmts -> do
        pushSymTab
        stmts' <- mapM instStmt stmts
        popSymTab
        return (Block stmts')

    Let pos pat Nothing mblk -> do
        pat' <- instPatternIsolated pat
        mblk' <- traverse instStmt mblk
        return $ Let pos pat' Nothing mblk'

    Let pos pat mexpr mblk -> do
        pat' <- instPattern pat
        mexpr' <- traverse instExpr mexpr
        mblk' <- traverse instStmt mblk
        return $ Let pos pat' mexpr' mblk'

    For pos expr mcnd blk -> do
        expr' <- instExpr expr
        mcnd' <- traverse instPattern mcnd
        blk'  <- instStmt blk
        return $ For pos expr' mcnd' blk'

    While pos cnd blk -> do
        cnd' <- instExpr cnd
        blk' <- instStmt blk
        return $ While pos cnd' blk'

    If pos expr true mfalse -> do
        expr' <- instExpr expr
        true' <- instStmt true
        mfalse' <- traverse (instStmt) mfalse
        return $ If pos expr' true' mfalse'

    Switch pos expr cases -> do
        expr' <- instExpr expr
        cases' <- forM cases $ \(pat, stmt) -> do
            pushSymTab
            pat' <- instPattern pat
            stmt' <- instStmt stmt
            popSymTab
            return (pat', stmt')
        return $ Switch pos expr' cases'

    Data pos symbol typ mexpr -> do
        mexpr' <- traverse instExpr mexpr
        return $ Data pos symbol typ mexpr'

    _ -> error (show stmt)



genSymbol :: String -> DoM InstantiatorState Symbol
genSymbol sym = do  
    modName <- gets (moduleName . astResolved)
    im <- gets $ Map.lookup sym . symSupply . astResolved
    let n = maybe 0 (id) im
    modifyAST $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return (SymResolved modName sym n)


instantiatorMapper :: Elem -> DoM InstantiatorState Elem
instantiatorMapper elem = case elem of
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) | all isAnnotated exprs -> do
        symbol' <- withPos pos $ resolveFuncCall symbol (map typeof exprs) exprType
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    ElemPattern (PatAnnotated (PatIdent pos symbol) patType) -> do
        void $ resolveFuncCall (Sym "set") [patType, patType] Void
        return elem

    ElemPattern (PatAnnotated (PatLiteral expr) patType) | isAnnotated expr -> do
        void $ resolveFuncCall (Sym "equal") [patType, patType] Type.Bool
        return elem

    ElemPattern (PatAnnotated (PatTuple pos pats) patType) | all patAnnotated pats -> do
        when (length pats > 0) $ void $ resolveFuncCall (Sym "first") [patType] (typeof $ pats !! 0)
        when (length pats > 1) $ void $ resolveFuncCall (Sym "second") [patType] (typeof $ pats !! 1)
        when (length pats > 2) $ void $ resolveFuncCall (Sym "third") [patType] (typeof $ pats !! 2)
        when (length pats > 3) $ void $ resolveFuncCall (Sym "fourth") [patType] (typeof $ pats !! 3)
        return elem

    ElemPattern (PatAnnotated (PatSlice pos pats) patType) -> do
        when (length pats > 0) $ do
            void $ resolveFuncCall (Sym "at") [patType, I64] (typeof $ head pats)
        void $ resolveFuncCall (Sym "len") [patType] I64
        return elem

    ElemPatternIsolated (PatAnnotated (PatIdent pos symbol) patType) -> do
        return elem

    ElemPatternIsolated (PatAnnotated (PatSlice pos pats) patType) -> do
        fail "what"
        return elem

    _ -> return elem
    where
        patAnnotated :: AST.Pattern -> Bool
        patAnnotated (PatAnnotated _ t) = True
        patAnnotated _                  = False


        isAnnotated :: AST.Expr -> Bool
        isAnnotated (AExpr _ _) = True
        isAnnotated _           = False


resolveFuncCall :: Symbol -> [Type] -> Type -> DoM InstantiatorState Symbol
resolveFuncCall s@(SymResolved _ _ _) argTypes retType = return s
resolveFuncCall calledSymbol        argTypes retType = do

    let callHeader = CallHeader calledSymbol argTypes retType

    funcDefs <- gets (Map.map funcHeader . funcDefs . astResolved)
    funcImports <- gets (Map.map funcHeader . funcImports . astResolved)

    let headers = Map.elems $ Map.unions [funcDefs, funcImports]

    candidates <- findCandidates callHeader headers
    ast <- gets astResolved

    case candidates of
        [] -> fail $ "no candidates for: " ++ show callHeader

        [FuncHeader _ generics symbol _ _] | generics == [] -> return symbol
        [FuncHeader _ generics symbol _ _] -> do -- this is where we replace
            funcReplaced <- replaceGenericsInFuncWithCall
                (getFunction symbol ast)
                callHeader

            case funcHeaderFullyResolved (funcHeader funcReplaced) of
                False -> return calledSymbol
                True  -> do
                    instancem <- findInstance ast $ CallHeader
                        symbol
                        (map typeof $ funcArgs $ funcHeader funcReplaced)
                        (typeof $ funcRetty $ funcHeader funcReplaced)
                    case instancem of
                        Just s -> return s
                        Nothing -> do
                            symbol' <- genSymbol (Symbol.sym calledSymbol)
                            modifyAST $ \s -> s { funcInstances = Map.insert symbol' funcReplaced (funcInstances s) }
                            return symbol'

        _ -> return (calledSymbol)
