module ResolveAst where

import Prelude hiding (mod)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Char

import Type
import AST
import Monad
import Error
import Symbol
import ASTResolved hiding (genSymbol)
import ASTMapper


data SymKey
    = KeyFunc
    | KeyType
    | KeyVar
    deriving (Ord, Eq, Show)


type MySymTab = [Set.Set (Symbol, SymKey)]


data ResolveState
    = ResolveState
        { modName :: String
        , symTab  :: MySymTab
        , supply  :: Map.Map Symbol Int
        }

initResolveState = ResolveState
    { modName = ""
    , symTab = [Set.empty]
    , supply = Map.empty
    }


pushSymbolTable :: DoM ResolveState ()
pushSymbolTable = do
    modify $ \s -> s { symTab = Set.empty : symTab s }


popSymbolTable :: DoM ResolveState ()
popSymbolTable = do
    modify $ \s -> s { symTab = tail (symTab s) }


printSymbolTable :: DoM ResolveState ()
printSymbolTable = do
    symTab <- gets symTab
    liftIO $ do
        forM_ symTab $ \set -> do
            putStrLn "scope:"
            forM_ (Set.toList set) $ \(s, k) -> do
                putStrLn $ "\t" ++ show k ++ " " ++ show s


lookupSymTab :: Symbol -> SymKey -> MySymTab -> [Symbol]
lookupSymTab symbol key []       = []
lookupSymTab symbol key (s : ss) = case symbol of
    Sym str -> case Set.toList (Set.filter (\(s, k) -> key == k && symbolsCouldMatch (Sym str) s) s) of
        [] -> lookupSymTab symbol key ss
        xs -> map fst xs

    SymResolved _ -> case Set.member (symbol, key) s of
        True -> [symbol]
        False -> lookupSymTab symbol key ss

    where
        removeGeneric :: String -> String
        removeGeneric str = reverse $ takeWhile (/= '>') (reverse str)


lookm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookm symbol key = do
    ress <- gets $ lookupSymTab symbol key . symTab
    case ress of
        [] -> return Nothing
        [x] -> return (Just x)
        _   -> fail ("multiple definitions for: " ++ prettySymbol symbol)
    

lookHeadm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookHeadm symbol key = do
    symTabHead <- gets (head . symTab)
    case lookupSymTab symbol key [symTabHead] of
        [] -> return Nothing
        [x] -> return (Just x)
        _   -> fail ("multiple definitions for: " ++ prettySymbol symbol)


look :: Symbol -> SymKey -> DoM ResolveState Symbol
look symbol key = do
    resm <- lookm symbol key
    unless (isJust resm) $ do
        (fail $ "undefined symbol: " ++ prettySymbol symbol)
    return (fromJust resm)


define :: Symbol -> SymKey -> DoM ResolveState ()
define symbol@(SymResolved _) key = do
    --liftIO $ putStrLn $ "defining: " ++ prettySymbol symbol
    resm <- lookHeadm symbol key
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symTab = (Set.insert (symbol, key) $ head $ symTab s) : (tail $ symTab s) }


genSymbol :: Symbol -> DoM ResolveState Symbol
genSymbol symbol@(SymResolved str) = do
    modName <- gets modName
    resm <- gets (Map.lookup symbol . supply)
    let n = maybe 0 (id) resm
    modify $ \s -> s { supply = Map.insert symbol (n + 1) (supply s) }
    return $ SymResolved $ [modName] ++ str ++ [show n]


resolveAst :: AST -> [ASTResolved] -> DoM s (AST, Map.Map Symbol Int)
resolveAst ast imports = fmap fst $ runDoMExcept initResolveState (resolveAst' ast)
    where
        resolveAst' :: AST -> DoM ResolveState (AST, Map.Map Symbol Int)
        resolveAst' ast = do
            modify $ \s -> s { modName = astModuleName ast }
            forM_ imports $ \imprt -> do
                forM_ (typeDefsTop imprt) $ \symbol -> define symbol KeyType

            
            -- pre-define top-level symbols
            topStmts' <- forM (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics (Sym str) typ -> do
                    symbol <- genSymbol (SymResolved str)
                    define symbol KeyType
                    return $ Typedef pos generics symbol typ

                FuncDef (AST.Func header stmt) -> do
                    symbol <- genSymbol (SymResolved $ symStr $ funcSymbol header)
                    define symbol KeyFunc
                    return $ FuncDef $ AST.Func (header { funcSymbol = symbol }) stmt

                Feature pos symbol arg headers -> do
                    symbol' <- genSymbol (SymResolved $ symStr symbol)
                    define symbol' KeyType

                    headers' <- forM headers $ \header -> do
                        fnSymbol' <- genSymbol $
                            SymResolved (tail (symStr symbol') ++ symStr (funcSymbol header))
                        define fnSymbol' KeyFunc
                        return $ header { funcSymbol = fnSymbol' }

                    return (Feature pos symbol' arg headers')

                _ -> return stmt


            stmts' <- mapM resolveStmt topStmts'
            supply <- gets supply
            return (ast { astStmts = stmts' }, supply)


defineGenerics :: [Symbol] -> DoM ResolveState [Symbol]
defineGenerics generics = forM generics $ \(Sym str) -> do
    symbol <- genSymbol $ SymResolved ("type" : str)
    define symbol KeyType
    return symbol


resolveParam :: Param -> DoM ResolveState Param
resolveParam param = withPos param $ case param of
    Param pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar
        Param pos symbol <$> resolveType typ
    RefParam pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar
        RefParam pos symbol <$> resolveType typ


resolveRetty :: Retty -> DoM ResolveState Retty
resolveRetty retty = case retty of
    RefRetty typ -> RefRetty <$> resolveType typ
    Retty typ    -> Retty <$> resolveType typ


resolveType :: Type -> DoM ResolveState Type
resolveType typ = case typ of
    TypeDef s      -> case s of
        Sym ["Array"] -> return Type.Array
        Sym ["Table"] -> return Table
        Sym ["Sum"]   -> return Sum
        Sym ["Tuple"] -> return Tuple
        _             -> TypeDef <$> look s KeyType

    Apply t ts -> do
        t' <- resolveType t
        ts' <- mapM resolveType ts
        return (Apply t' ts')

    _ -> return typ


resolveStmt :: Stmt -> DoM ResolveState Stmt
resolveStmt statement = withPos statement $ case statement of
    Typedef pos generics symbol typ -> do
        symbol' <- case symbol of
            SymResolved _ -> return symbol
            Sym str       -> do
                s <- genSymbol (SymResolved str)
                define s KeyType
                return s

        pushSymbolTable
        generics' <- defineGenerics generics
        typ' <- resolveType typ
        popSymbolTable
        return (Typedef pos generics' symbol' typ')

    Feature pos symbol arg headers -> do
        symbol' <- case symbol of
            SymResolved _ -> return symbol
            Sym str -> do
                s <- genSymbol (SymResolved str)
                define s KeyType
                return s

        headers' <- forM headers $ \header -> do
            fnSymbol' <- case funcSymbol header of
                SymResolved _ -> return (funcSymbol header)
                Sym str -> do
                    s <- genSymbol $ SymResolved (tail (symStr symbol') ++ str)
                    define s KeyFunc
                    return s
            return $ header { funcSymbol = fnSymbol' }

        pushSymbolTable

        arg' <- head <$> defineGenerics [arg]

        headers'' <- forM headers' $ \header -> do
            pushSymbolTable
            fnGenerics' <- defineGenerics (funcGenerics header)
            fnArgs' <- mapM resolveParam (funcArgs header)
            fnRetty' <- resolveRetty (funcRetty header)
            popSymbolTable
            return $ header { funcArgs = fnArgs', funcGenerics = fnGenerics', funcRetty = fnRetty' }

        popSymbolTable

        return (Feature pos symbol' arg' headers'')

    FuncDef (AST.Func header stmt) -> do
        symbol' <- case (funcSymbol header) of
            SymResolved _ -> return (funcSymbol header)
            Sym str       -> do
                s <- genSymbol (SymResolved $ symStr $ funcSymbol header)
                define s KeyFunc
                return s

        pushSymbolTable
        generics' <- defineGenerics (funcGenerics header)
        args'     <- mapM resolveParam (funcArgs header)
        retty'    <- resolveRetty (funcRetty header)
        stmt'     <- resolveStmt stmt

        let header' = header
                { funcGenerics = generics'
                , funcArgs     = args'
                , funcRetty    = retty'
                , funcSymbol   = symbol'
                }
        popSymbolTable
        return $ FuncDef (AST.Func header' stmt')

    Block stmts -> do
        pushSymbolTable
        stmts' <- mapM resolveStmt stmts
        popSymbolTable
        return (Block stmts')

    Let pos pat Nothing mblk -> do
        when (isJust mblk) pushSymbolTable
        pat' <- resolvePattern pat
        mblk' <- traverse resolveStmt mblk
        when (isJust mblk) popSymbolTable
        return (Let pos pat' Nothing mblk')

    If pos expr stmt melse -> do
        pushSymbolTable
        expr' <- resolveExpr expr
        stmt' <- resolveStmt stmt
        popSymbolTable
        pushSymbolTable
        melse' <- traverse resolveStmt melse
        popSymbolTable
        return (If pos expr' stmt' melse')

    While pos expr stmt -> do
        pushSymbolTable
        expr' <- resolveExpr expr
        stmt' <- resolveStmt stmt
        popSymbolTable
        return (While pos expr' stmt')
    
    Data pos (Sym str) typ mexpr -> do
        symbol <- genSymbol (SymResolved str)
        define symbol KeyVar
        typ' <- resolveType typ
        mexpr' <- traverse resolveExpr mexpr
        return (Data pos symbol typ' mexpr')

    Return pos mexpr -> Return pos <$> traverse resolveExpr mexpr
    EmbedC pos str -> EmbedC pos <$> processCEmbed str
    ExprStmt expr -> ExprStmt <$> resolveExpr expr

    Assign pos (Sym str) expr -> do
        symbol <- genSymbol (SymResolved str)
        define symbol KeyVar
        Assign pos symbol <$> resolveExpr expr

    x -> error (show x)


resolvePattern :: Pattern -> DoM ResolveState Pattern
resolvePattern pattern = withPos pattern $ case pattern of
    PatAnnotated pat typ -> do
        typ' <- resolveType typ
        pat' <- resolvePattern pat
        return (PatAnnotated pat' typ')

    PatIdent pos (Sym str) -> do
        symbol <- genSymbol (SymResolved str)
        define symbol KeyVar
        return (PatIdent pos symbol)

    x -> error (show x)


resolveExpr :: Expr -> DoM ResolveState Expr
resolveExpr expression = withPos expression $ case expression of
    AExpr typ expr -> do
        expr' <- resolveExpr expr
        typ' <- resolveType typ
        return (AExpr typ' expr')
    AST.Int pos n -> return (AST.Int pos n)
    AST.Bool pos b -> return (AST.Bool pos b)
    AST.Char pos c -> return (AST.Char pos c)
    AST.Float pos f -> return (AST.Float pos f)
    Ident pos symbol -> Ident pos <$> look symbol KeyVar
    Reference pos expr -> Reference pos <$> resolveExpr expr
    Field pos expr n -> do
        expr' <- resolveExpr expr
        return (Field pos expr' n)
    Match pos expr pat -> do
        expr' <- resolveExpr expr
        pat' <- resolvePattern pat
        return (Match pos expr' pat')
    AST.String pos s -> return (AST.String pos s)
    AST.Array pos exprs -> AST.Array pos <$> mapM resolveExpr exprs
    Call pos symbol exprs -> Call pos symbol <$> mapM resolveExpr exprs

    x -> error (show x)
            

processCEmbed :: String -> DoM ResolveState String
processCEmbed ('$':xs) = do
    let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
    check (length ident > 0)     "invalid identifier following '$' token"
    check (isAlpha $ ident !! 0) "invalid identifier following '$' token"
    let rest = drop (length ident) xs

    symbol <- look (Sym [ident]) KeyVar
    (showSymLocal symbol ++) <$> processCEmbed rest
processCEmbed (x:xs) = (x:) <$> processCEmbed xs
processCEmbed [] = return ""
