module Resolve2 where

import ASTResolved
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import AST
import Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Data.List
import Symbol
import Error
import qualified SymTab
import Type



data SymKey
    = KeyVar
    | KeyType
    | KeyFunc
    deriving (Ord, Eq, Show)


data ResolveState = ResolveState
    { symTab :: SymTab.SymTab Symbol SymKey ()
    , modName :: String
    , supply :: Map.Map (Symbol, SymKey) Int
    }

initResolveState name = ResolveState
    { symTab = SymTab.initSymTab
    , modName = name
    , supply = Map.empty
    }


popSymTab :: DoM ResolveState ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


pushSymTab :: DoM ResolveState ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }


resolveAst :: AST -> [ASTResolved] -> DoM s ASTResolved
resolveAst ast imports = do
    ast' <- fmap fst $ runDoMExcept (initResolveState $ astModuleName ast) (resolveSymbols ast)
    liftIO $ prettyAST ast'
    error "here"


look :: Symbol -> SymKey -> DoM ResolveState Symbol
look symbol key = do
    resm <- lookm symbol key
    case resm of
        Nothing -> do
            st <- gets symTab 
            liftIO $ SymTab.prettySymTab st
            
            fail $ prettySymbol symbol ++ " isn't defined"
        Just x  -> return x


lookm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookm symbol key = gets (SymTab.lookupWith (symbolCouldMatch symbol) key . symTab)

lookHeadm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookHeadm symbol key = gets (SymTab.lookupWith (symbolCouldMatch symbol) key . (:[]) . head . symTab)


symbolCouldMatch :: Symbol -> Symbol -> Bool
symbolCouldMatch s1 s2
    | isResolved s1 && isResolved s2 = isTailOf s1 s2 -- both symbols are resolved
    | otherwise                      = isTailOf (stripLevels s1) (stripLevels s2)
    where
        isTailOf :: Symbol -> Symbol -> Bool
        isTailOf s1 s2 = isPrefixOf (reverse $ parseStr $ symStr s1) (reverse $ parseStr $ symStr s2)

        stripLevels :: Symbol -> Symbol
        stripLevels symbol = symbol
            { symStr = intercalate "::" (filter (\x -> not (isDigit (head x))) (parseStr $ symStr symbol)) }



symbolName :: Symbol -> String
symbolName symbol = case reverse (parseStr $ symStr symbol) of
    (x : xs) | isDigit (head x) -> head xs
    xs                          -> head xs




define :: Symbol -> SymKey -> DoM ResolveState Symbol
define symbol@(Symbol2Resolved _) key = do
    symbol' <- supplySymbol symbol key
    resm <- lookHeadm symbol key
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symTab = SymTab.insert symbol' key () (symTab s) }
    return symbol'



supplySymbol :: Symbol -> SymKey -> DoM ResolveState Symbol
supplySymbol symbol key = do
    resm <- gets $ Map.lookup (symbol, key) . supply
    case resm of
        Nothing -> do
            modify $ \s -> s { supply = Map.insert (symbol, key) 1 (supply s) }
            return $ Symbol2Resolved (symStr symbol)
        Just n  -> do
            modify $ \s -> s { supply = Map.insert (symbol, KeyType) (n + 1) (supply s) }
            return $ Symbol2Resolved (symStr symbol ++ "::" ++ show n)


defineNewType :: Symbol -> DoM ResolveState Symbol
defineNewType symbol@(Symbol2 str) = do
    unless (length (parseStr str) == 1) (error "symbol must be one ident")
    modName <- gets modName
    let symbol' = Symbol2Resolved (modName ++ "::" ++ str)
    define symbol' KeyType


defineNewFeature :: Symbol -> DoM ResolveState Symbol
defineNewFeature symbol@(Symbol2 str) = do
    unless (length (parseStr str) == 1) (error "symbol must be one ident")
    modName <- gets modName
    let symbol' = Symbol2Resolved (modName ++ "::" ++ str)
    define symbol' KeyType


defineNewFeatureMember :: Symbol -> Symbol -> DoM ResolveState Symbol
defineNewFeatureMember featureSymbol symbol@(Symbol2 str) = do
    unless (length (parseStr str) == 1) (error "symbol must be one ident")
    let symbol' = Symbol2Resolved (symStr featureSymbol ++ "::" ++ symStr symbol)
    define symbol' KeyFunc


defineNewFunc :: Symbol -> DoM ResolveState Symbol
defineNewFunc symbol@(Symbol2 str) = do
    unless (length (parseStr str) == 1) (error "symbol must be one ident")
    modName <- gets modName
    let symbol' = Symbol2Resolved (modName ++ "::" ++ str)
    define symbol' KeyFunc


defineNewVar :: Symbol -> DoM ResolveState Symbol
defineNewVar symbol@(Symbol2 str) = do
    unless (length (parseStr str) == 1) (error "symbol must be one ident")
    modName <- gets modName
    let symbol' = Symbol2Resolved (modName ++ "::" ++ str)
    define symbol' KeyVar


defineNewGenerics :: [Symbol] -> DoM ResolveState [Symbol]
defineNewGenerics symbols = do
    forM symbols $ \symbol -> do
        unless (length (parseStr $ symStr symbol) == 1) (error "symbol must be one ident")
        let symbol' = Symbol2Resolved ("type::" ++ symStr symbol)
        define symbol' KeyType
    

resolveSymbols :: AST -> DoM ResolveState AST
resolveSymbols ast = do
    forM_ (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
        FuncDef _       -> return ()
        Typedef _ _ _ _ -> return ()
        Feature _ _ _   -> return ()
        _               -> fail "invalid top-level statement"

    let typeDefs = [ x | x@(Typedef _ _ _ _) <- astStmts ast ]
    let features = [ x | x@(Feature _ _ _)   <- astStmts ast ]
    let funcDefs = [ x | x@(FuncDef _)       <- astStmts ast ]

    typeDefs' <- forM typeDefs $ \(Typedef pos generics symbol typ) -> do
        symbol' <- defineNewType symbol
        return (Typedef pos generics symbol' typ)

    features' <- forM features $ \(Feature pos symbol headers) -> do
        symbol' <- defineNewFeature symbol
        return (Feature pos symbol' headers)

    funcDefs' <- forM funcDefs $ \(FuncDef (Func header stmt)) -> do
        symbol' <- case isQualified (funcSymbol header) of
            False -> defineNewFunc (funcSymbol header)
            True  -> look (funcSymbol header) KeyFunc
        return $ FuncDef $ Func (header { funcSymbol = symbol' }) stmt

    stmts' <- mapM resolveStmt (typeDefs' ++ features' ++ funcDefs')
    return (ast { astStmts = stmts' })


resolveArg :: Param -> DoM ResolveState Param
resolveArg arg = withPos arg $ case arg of
    Param pos symbol typ -> do
        symbol' <- defineNewVar symbol
        typ' <- resolveType typ
        return $ Param pos symbol' typ'
    x -> error (show x)


resolveRetty :: Retty -> DoM ResolveState Retty
resolveRetty retty = case retty of
    Retty typ -> Retty <$> resolveType typ


resolveStmt :: Stmt -> DoM ResolveState Stmt
resolveStmt statement = withPos statement $ case statement of
    FuncDef (Func header stmt) -> do
        symbol' <- case isQualified (funcSymbol header) of
            True -> look (funcSymbol header) KeyFunc
            False -> defineNewFunc (funcSymbol header)

        pushSymTab
        generics' <- defineNewGenerics (funcGenerics header)
        args' <- mapM resolveArg (funcArgs header)
        retty' <- resolveRetty (funcRetty header)
        stmt' <- resolveStmt stmt
        let header' = FuncHeader
                { funcSymbol = symbol'
                , funcArgs   = args'
                , funcRetty  = retty'
                , funcGenerics = generics'
                , funcPos      = funcPos header
                }
        popSymTab
        return $ FuncDef (Func header' stmt')

    Feature pos symbol headers -> do
        symbol' <- case isResolved symbol of
            True -> look symbol KeyType
            False -> defineNewFeature symbol
        headers' <- forM headers $ \header -> withPos header $ do
            funcSymbol' <- defineNewFeatureMember symbol' (funcSymbol header)
            pushSymTab
            generics' <- defineNewGenerics (funcGenerics header)
            args' <- mapM resolveArg (funcArgs header)
            retty' <- resolveRetty (funcRetty header)
            let header' = FuncHeader
                    { funcSymbol = funcSymbol'
                    , funcArgs   = args'
                    , funcRetty  = retty'
                    , funcGenerics = generics'
                    , funcPos      = funcPos header
                    }
            popSymTab
            return header'
        return (Feature pos symbol' headers')
        
    Typedef pos generics symbol typ -> do
        symbol' <- case isResolved symbol of
            True -> look symbol KeyType
            False -> defineNewType symbol
        Typedef pos generics symbol' <$> resolveType typ

    ExprStmt expr -> do
        expr' <- resolveExpr expr
        return $ ExprStmt expr'

    Block stmts -> do
        pushSymTab
        stmts' <- mapM resolveStmt stmts
        popSymTab
        return $ Block stmts'

    Let pos pattern Nothing Nothing -> do
        pattern' <- resolvePattern pattern
        return $ Let pos pattern' Nothing Nothing

    Return pos mexpr -> do
        Return pos <$> traverse resolveExpr mexpr
    

    a -> error (show a)


resolvePattern :: Pattern -> DoM ResolveState Pattern
resolvePattern pattern = withPos pattern $ case pattern of
    PatIdent pos symbol -> do
        symbol' <- defineNewVar symbol
        return $ PatIdent pos symbol'

    PatAnnotated pat typ -> do
        typ' <- resolveType typ
        pat' <- resolvePattern pat
        return $ PatAnnotated pat' typ'


resolveExpr :: Expr -> DoM ResolveState Expr
resolveExpr expression = withPos expression $ case expression of
    Ident pos symbol -> do
        symbol' <- look symbol KeyVar
        return $ Ident pos symbol'

    Call pos symbol exprs -> do
        symbol' <- look symbol KeyFunc
        Call pos symbol' <$> mapM resolveExpr exprs

    x -> error (show x)


resolveType :: Type -> DoM ResolveState Type
resolveType typ = case typ of
    Void           -> return typ
    x | isSimple x -> return typ
    TypeApply symbol ts -> do
        symbol' <- look symbol KeyType
        TypeApply symbol' <$> mapM resolveType ts


    x -> error (show x)

