module ResolveAst where

import Prelude hiding (mod)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe
import Data.Char

import Type
import AST
import Monad
import Error
import Symbol
import ASTResolved hiding (genSymbol)


data SymKey
    = KeyType
    | KeyVar
    deriving (Ord, Eq, Show)


type MySymTab = [Map.Map (Symbol, SymKey, Bool) Symbol]

data ResolveState
    = ResolveState
        { modName   :: String
        , supply    :: Map.Map Symbol Int
        , symTab    :: MySymTab
        }

initResolveState = ResolveState
    { modName = ""
    , symTab = [Map.empty]
    , supply = Map.empty
    }


pushSymbolTable :: DoM ResolveState ()
pushSymbolTable = do
    modify $ \s -> s { symTab = Map.empty : symTab s }


popSymbolTable :: DoM ResolveState ()
popSymbolTable = do
    modify $ \s -> s { symTab = tail (symTab s) }


printSymbolTable :: DoM ResolveState ()
printSymbolTable = do
    symTab <- gets symTab
    liftIO $ do
        forM_ symTab $ \mp -> do
            putStrLn "scope:"
            forM_ (Map.toList mp) $ \(s, k) -> do
                putStrLn $ "\t" ++ show k ++ " " ++ show s


lookupSymTab :: Symbol -> SymKey -> MySymTab -> [Symbol]
lookupSymTab symbol key []       = []
lookupSymTab symbol key (s : ss) = 
    case Map.elems (Map.filterWithKey (\(s, k, q) _ -> key == k && match (s, q) symbol) s) of
        [] -> lookupSymTab symbol key ss
        xs -> xs
    where
        match :: (Symbol, Bool) -> Symbol -> Bool
        match (s, isQualified) symbol = case (isQualified, symbol) of
            (True, Sym [_])    -> False
            (True, Sym [_, _]) -> symbolsCouldMatch symbol s
            (True, SymResolved [_, _]) -> symbolsCouldMatch symbol s
            (False, _)         -> symbolsCouldMatch symbol s
            x                  -> error (show x)


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


define :: Symbol -> SymKey -> Symbol -> Bool -> DoM ResolveState ()
define symbol@(SymResolved _) key symbol2 isQualified = do
    --liftIO $ putStrLn $ "defining: " ++ prettySymbol symbol
    resm <- lookHeadm symbol key
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symTab = (Map.insert (symbol, key, isQualified) symbol2 $ head $ symTab s) : (tail $ symTab s) }


genGeneric :: Symbol -> DoM ResolveState Symbol
genGeneric symbol@(SymResolved str) = do
    modName <- gets modName
    resm <- gets (Map.lookup symbol . supply)
    let n = maybe 0 (id) resm
    modify $ \s -> s { supply = Map.insert symbol (n + 1) (supply s) }
    case n of
        0 -> return $ SymResolved $ [modName] ++ str
        n -> return $ SymResolved $ [modName] ++ str ++ [show n]

genSymbol :: Symbol -> DoM ResolveState Symbol
genSymbol symbol@(SymResolved str) = do
    modName <- gets modName
    resm <- gets (Map.lookup symbol . supply)
    let n = maybe 0 (id) resm
    modify $ \s -> s { supply = Map.insert symbol (n + 1) (supply s) }
    case n of
        0 -> return $ SymResolved $ [modName] ++ str
        n -> return $ SymResolved $ [modName] ++ str ++ [show n]


resolveAst :: AST -> [(Import, ASTResolved)] -> DoM s (AST, Map.Map Symbol Int)
resolveAst ast imports = fmap fst $ runDoMExcept initResolveState (resolveAst' ast)
    where
        resolveAst' :: AST -> DoM ResolveState (AST, Map.Map Symbol Int)
        resolveAst' ast = do
            modify $ \s -> s { modName = astModuleName ast }

            forM_ imports $ \(Import isExport isQualified path mName, imprt) -> do
                forM_ (typeDefsTop imprt) $ \symbol -> do
                    let SymResolved [mod, name] = symbol

                    symbol' <- case mName of
                        Nothing -> return symbol
                        Just n  -> return $ SymResolved [n, name]

                    define symbol' KeyType symbol isQualified


            pushSymbolTable

            -- pre-define top-level symbols
            topStmts' <- forM (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics (Sym str) typ -> do
                    symbol' <- genSymbol (SymResolved str)
                    define symbol' KeyType symbol' False
                    return (Typedef pos generics symbol' typ)

                Feature pos generics funDeps symbol args retty -> do
                    symbol' <- genSymbol (SymResolved $ symStr symbol)
                    define symbol' KeyType symbol' False
                    return (Feature pos generics funDeps symbol' args retty)

                _ -> return stmt


            stmts' <- mapM resolveStmt topStmts'
            supply <- gets supply

            popSymbolTable
            return (ast { astStmts = stmts' }, supply)


defineGenerics :: [Symbol] -> DoM ResolveState [Symbol]
defineGenerics generics = forM generics $ \(Sym str) -> do
    symbol <- genGeneric $ SymResolved str
    define symbol KeyType symbol False
    return symbol


resolveParam :: Param -> DoM ResolveState Param
resolveParam param = withPos param $ case param of
    Param pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar symbol False
        Param pos symbol <$> resolveType typ
    RefParam pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar symbol False
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
        Sym ["Slice"] -> return Slice
        _             -> TypeDef <$> look s KeyType

    Apply t1 t2 -> do
        t1' <- resolveType t1
        t2' <- resolveType t2
        return (Apply t1' t2')

    _ -> return typ


resolveStmt :: Stmt -> DoM ResolveState Stmt
resolveStmt statement = withPos statement $ case statement of
    Return pos mexpr -> Return pos <$> traverse resolveExpr mexpr
    Typedef pos generics symbol typ -> do
        symbol' <- case symbol of
            SymResolved _ -> return symbol
            Sym str       -> do
                s <- genSymbol (SymResolved str)
                define s KeyType symbol False
                return s

        pushSymbolTable
        generics' <- defineGenerics generics
        typ' <- resolveType typ
        popSymbolTable
        return (Typedef pos generics' symbol' typ')

    Feature pos generics funDeps symbol args retty -> do
        unless (symbolIsResolved symbol) (fail "feature symbol wasn't resolved")
        pushSymbolTable
        generics' <- defineGenerics generics
        funDeps' <- forM funDeps $ \(a, b) -> do
            let [a'] = filter (symbolsCouldMatch a) generics'
            let [b'] = filter (symbolsCouldMatch b) generics'
            return (a', b')
        args' <- mapM resolveType args
        retty' <- resolveType retty
        popSymbolTable
        return (Feature pos generics' funDeps' symbol args' retty')

    Derives pos generics t1 ts -> do
        pushSymbolTable
        generics' <- defineGenerics generics
        t1' <- resolveType t1
        ts' <- mapM resolveType ts
        popSymbolTable
        return (Derives pos generics' t1' ts')

    Acquires pos generics typ args isRef stmt -> do
        pushSymbolTable
        generics' <- defineGenerics generics
        typ' <- resolveType typ
        args' <- mapM resolveParam args
        stmt' <- resolveStmt stmt

        popSymbolTable
        return (Acquires pos generics' typ' args' isRef stmt')

    Block stmts -> do
        pushSymbolTable
        stmts' <- mapM resolveStmt stmts
        popSymbolTable
        return (Block stmts')

    Let pos pat mexpr mblk -> do
        when (isJust mblk) pushSymbolTable
        pat' <- resolvePattern pat
        mexpr' <- traverse resolveExpr mexpr
        mblk' <- traverse resolveStmt mblk
        when (isJust mblk) popSymbolTable
        return (Let pos pat' mexpr' mblk')

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
        define symbol KeyVar symbol False
        typ' <- resolveType typ
        mexpr' <- traverse resolveExpr mexpr
        return (Data pos symbol typ' mexpr')
    EmbedC pos [] str -> do 
        map <- processCEmbed str
        return (EmbedC pos map str)
    ExprStmt expr -> ExprStmt <$> resolveExpr expr

    Assign pos (Sym str) expr -> do
        symbol <- genSymbol (SymResolved str)
        define symbol KeyVar symbol False
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
        define symbol KeyVar symbol False
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
    Match pos expr pat -> do
        expr' <- resolveExpr expr
        pat' <- resolvePattern pat
        return (Match pos expr' pat')
    AST.String pos s -> return (AST.String pos s)
    AST.Array pos exprs -> AST.Array pos <$> mapM resolveExpr exprs

    Call pos typ exprs -> do
        typ' <- resolveType typ
        Call pos typ' <$> mapM resolveExpr exprs

    x -> error (show x)
            

processCEmbed :: String -> DoM ResolveState [(String, Symbol)]
processCEmbed ('$':xs) = do
    let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
    check (length ident > 0)     "invalid identifier following '$' token"
    check (isAlpha $ ident !! 0) "invalid identifier following '$' token"
    let rest = drop (length ident) xs
    symbol <- look (Sym [ident]) KeyVar
    ((ident, symbol) :) <$> processCEmbed rest

processCEmbed (x:xs) = processCEmbed xs
processCEmbed [] = return []
