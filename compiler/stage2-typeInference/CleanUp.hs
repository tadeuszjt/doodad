module CleanUp where

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


-- Resolves function calls
-- Creates generic instantiations
-- Resolves tuple/table field symbols
-- Turns ctor function call into Contructors
-- Removed spurious tuple types
compile :: Bool -> DoM ASTResolved ()
compile verbose = do
    --when verbose $ liftIO $ putStrLn $ "cleaning..."
    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            stmt' <- (mapStmtM cleanUpMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }


genSymbol :: String -> DoM ASTResolved Symbol
genSymbol sym = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup sym . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return (SymResolved modName sym n)


cleanUpMapper :: Elem -> DoM ASTResolved Elem
cleanUpMapper elem = case elem of
    ElemType t -> return (ElemType t)
        
    ElemExpr (AExpr exprType expr@(AST.Field pos e symbol)) -> case symbol of
        Sym _             -> ElemExpr . AExpr exprType . AST.Field pos e <$> resolveFieldAccess (typeof e) symbol
        SymResolved _ _ _ -> return $ ElemExpr $ AExpr exprType (Field pos e symbol)

    ElemExpr (AExpr exprType expr@(AST.Call pos mparam symbol exprs)) -> do
        symbol' <- resolveFuncCall exprType expr
        isCtor <- Map.member symbol' <$> gets ctorDefs
        fmap (ElemExpr . AExpr exprType) $ case isCtor of
            False -> return (Call pos mparam symbol' exprs)
            True -> do
                unless (isNothing mparam) (error "invalid params")
                return (Construct pos symbol' exprs)

    ElemPattern (PatField pos symbol pats) -> do
        ast <- get
        [symbol'] <- fmap catMaybes $ forM (Map.keys $ ctorDefs ast) $ \symb ->
            case symbolsCouldMatch symb symbol of
                True -> return (Just symb)
                False -> return Nothing
        return $ ElemPattern (PatField pos symbol' pats)

    _ -> return elem

    
-- add extern if needed
resolveFuncCall :: Type -> AST.Expr -> DoM ASTResolved Symbol
resolveFuncCall _ (AST.Call _ _ s@(SymResolved _ _ _) _) = return s
resolveFuncCall exprType (AST.Call pos Nothing callSymbol args) = withPos pos $ do
    --liftIO $ putStrLn $ "resolving: " ++ show callSymbol
    let callHeader = CallHeader callSymbol (map typeof args) exprType

    candidates <- findCandidates callHeader
    ast <- get
    case candidates of
        [] -> fail $ "no candidates for: " ++ show callHeader

        [symbol] | isNonGenericFunction symbol ast -> return symbol
        [symbol] | isCtor symbol ast               -> return symbol
        [symbol] | isGenericFunction symbol ast    -> do -- this is where we replace
            let genericBody = getFunctionBody symbol ast
            bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
            --liftIO $ putStrLn $ "new body: " ++ show bodyReplaced
            case funcFullyResolved (funcGenerics genericBody) bodyReplaced of
                False -> return callSymbol
                True  -> do
                    symbol' <- genSymbol (Symbol.sym callSymbol)
                    modify $ \s -> s { funcDefs = Map.insert symbol' bodyReplaced (funcDefs s) }
                    return symbol'

        [genericSymbol, nonGenericSymbol] |
            isGenericFunction genericSymbol ast &&
            isNonGenericFunction nonGenericSymbol ast -> do
                let genericBody = getFunctionBody genericSymbol ast
                let nonGenericBody = getFunctionBody nonGenericSymbol ast
                bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
                case funcHeaderTypesMatch bodyReplaced nonGenericBody of
                    True -> return nonGenericSymbol
                    False -> return callSymbol

        [nonGenericSymbol, genericSymbol] |
            isGenericFunction genericSymbol ast &&
            isNonGenericFunction nonGenericSymbol ast -> do
                let nonGenericBody = getFunctionBody nonGenericSymbol ast
                let genericBody = getFunctionBody genericSymbol ast
                bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
                case funcHeaderTypesMatch bodyReplaced nonGenericBody of
                    True -> return nonGenericSymbol
                    False -> return callSymbol

        _ -> return callSymbol


-- (x:typ).sym -> (x:typ).mod_sym_0
resolveFieldAccess :: Type -> Symbol -> DoM ASTResolved Symbol
resolveFieldAccess (Type _) (Sym sym) = return (Sym sym)
resolveFieldAccess typ (Sym sym) = do
    --liftIO $ putStrLn $ "resolving field: " ++ sym
    typeDefs <- gets typeFuncs
    ctors    <- gets ctorDefs
    --liftIO $ putStrLn $ "resolveFieldAccess: " ++ sym ++ " " ++ show typ
    let typeSymbol = getFieldAccessorSymbol typeDefs typ
    --typeFieldSymbols <- getTypeFieldSymbols typ
    --typeResults <- return $ filter (\s -> Symbol.sym s == sym) typeFieldSymbols
    ctorResults <- return $ Map.keys $ Map.filterWithKey
        (\symbol (typSym, _) -> Symbol.sym symbol == sym && typeSymbol == typSym)
        ctors

    --case ctorResults ++ typeResults of
    case ctorResults of
        (a:b:xs) -> fail "ambiguous field symbol"
        []       -> return (Sym sym)
        [symbol] -> return symbol
    where
        -- Returns the symbol from the type which will be associated with field accesses. 
        getFieldAccessorSymbol :: TypeDefsMap -> Type -> Symbol
        getFieldAccessorSymbol typeDefs typ = case typ of
            TypeApply symbol ts -> case Map.lookup symbol typeDefs of
                Just (xs, t) -> symbol
                x -> error (show x)
            _ -> error (show typ)

        
        getTypeFieldSymbols :: Type -> DoM ASTResolved [Symbol]
        getTypeFieldSymbols typ = do
            base <- baseTypeOf typ
            case base of
                --Type.Tuple t@(TypeApply _ _) -> getTypeFieldSymbols t
                --Type.Table t@(TypeApply _ _) -> getTypeFieldSymbols t
                _ -> error (show typ)
            where
                isSymbolType :: Type -> DoM ASTResolved (Maybe Symbol)
                isSymbolType typ = case typ of
                    TypeApply s ts -> do
                        True <- Map.member s <$> getTypeDefs
                        return (Just s)
                    _ -> return Nothing

