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
    ast <- gets astResolved
    forM_ (funcDefsTop ast) $ \symbol -> do
        let func = (funcDefsAll ast) Map.! symbol
        let header = funcHeader func
        let isGeneric = isGenericHeader (funcHeader func)
        let callHeader = callHeaderFromFuncHeader (funcHeader func)
        let isInstantiated = Map.member callHeader (funcInstance ast)

        when (not isGeneric && not isInstantiated) $ do
            instanceSymbol <- liftASTState $ genSymbol $ SymResolved $ ["instance"] ++ symStr (funcSymbol header)
            let headerInstance = header { funcSymbol = instanceSymbol }
            modifyAST $ \s -> s
                { funcInstance = Map.insert callHeader (AST.Func headerInstance $ funcStmt func) (funcInstance s)
                } 

    funcInstances <- gets (funcInstance . astResolved)
    forM_ (Map.toList funcInstances) $ \(header, func) -> do
        stmt' <- mapStmtM instantiatorMapper (funcStmt func)
        func' <- return func { funcStmt = stmt' }
        modifyAST $ \s -> s { funcInstance = Map.insert header func' (ASTResolved.funcInstance s) }


instantiatorMapper :: Elem -> DoM InstantiatorState Elem
instantiatorMapper elem = case elem of
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) | all isAnnotated exprs -> do
        symbol' <- withPos pos $ resolveFuncCall symbol (map typeof exprs) exprType
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    _ -> return elem
    where
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
        [] -> fail ("no candidates for: " ++ show callHeader)

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
                            instanceSymbol <- liftASTState $ genSymbol $ SymResolved $
                                ["instance"] ++ symStr (funcSymbol $ funcHeader funcReplaced)
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
