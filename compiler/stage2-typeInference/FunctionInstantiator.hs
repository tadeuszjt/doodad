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
        }

initInstantiatorState ast = InstantiatorState
    { astResolved = ast
    }



liftASTState :: DoM ASTResolved a -> DoM InstantiatorState a
liftASTState m = do
    ast <- gets astResolved
    (a, ast') <- runDoMExcept ast m
    modify $ \s -> s { astResolved = ast' }
    return a


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
    funcInstances <- gets (funcInstance . astResolved)
    forM_ (Map.toList funcInstances) $ \(header, func) -> do
        stmt' <- instStmt (funcStmt func)
        func' <- return func { funcStmt = stmt' }
        modifyAST $ \s -> s { funcInstance = Map.insert header func' (ASTResolved.funcInstance s) }


instExpr :: Expr -> DoM InstantiatorState Expr
instExpr = mapExprM instantiatorMapper

instPattern :: Pattern -> DoM InstantiatorState Pattern
instPattern = mapPattern instantiatorMapper

instPatternIsolated :: Pattern -> DoM InstantiatorState Pattern
instPatternIsolated = mapPatternIsolated instantiatorMapper

instStmt :: Stmt -> DoM InstantiatorState Stmt
instStmt = mapStmtM instantiatorMapper


instantiatorMapper :: Elem -> DoM InstantiatorState Elem
instantiatorMapper elem = case elem of
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) | all isAnnotated exprs -> do
        symbol' <- withPos pos $ resolveFuncCall symbol (map typeof exprs) exprType
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    ElemPattern (PatAnnotated (PatIdent pos symbol) patType) -> do
        void $ resolveFuncCall (Sym "Store::store") [patType, patType] Void
        return elem

    ElemPattern (PatAnnotated (PatLiteral expr) patType) | isAnnotated expr -> do
        void $ resolveFuncCall (Sym "Compare::equal") [patType, patType] Type.Bool
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
resolveFuncCall calledSymbol argTypes retType
    | symbolIsResolved calledSymbol = return calledSymbol
resolveFuncCall calledSymbol      argTypes retType = do
    let callHeader = CallHeader calledSymbol argTypes retType
    headers <- gets (Map.elems . Map.map funcHeader . funcDefsAll . astResolved)
    candidates <- findCandidates callHeader headers
    ast <- gets astResolved

    case candidates of
        [] -> error ("no candidates for: " ++ prettySymbol calledSymbol)

        [header] -> do
            instancem <- findInstance ast $ CallHeader
                calledSymbol
                (map typeof $ funcArgs $ header)
                (typeof $ funcRetty $ header)

            case instancem of
                Just symbol -> return symbol
                Nothing -> do
                    funcReplaced <- replaceGenericsInFuncWithCall
                        (getFunction (funcSymbol header) ast)
                        callHeader

                    case funcHeaderFullyResolved (funcHeader funcReplaced) of
                        True -> do
                            instanceSymbol <- liftASTState $ genSymbol $
                                SymResolved ("instance_" ++ Symbol.sym calledSymbol)
                            modifyAST $ \s -> s { funcInstance = Map.insert
                                (callHeaderFromFuncHeader $ funcHeader funcReplaced)
                                (funcReplaced { funcHeader = (funcHeader funcReplaced)
                                    { funcSymbol = instanceSymbol
                                    }})
                                (funcInstance s) }

                            return instanceSymbol

                        False -> 
                            error "False"

        _ -> return calledSymbol
