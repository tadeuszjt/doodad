{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ResolveAst where

import Prelude hiding (mod)

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Maybe
import Data.Char

import Type
import AST
import Error
import Symbol
import ASTResolved hiding (genSymbol)
import AstBuilder
import InstBuilder


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


newtype Resolve a = Resolve
    { unResolve :: StateT ResolveState (StateT InstBuilderState (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState ResolveState, MonadError Error)


instance MonadFail Resolve where
    fail = throwError . ErrorStr


instance MonadInstBuilder Resolve where
    liftInstBuilderState (StateT s) = Resolve $ lift $ state (runIdentity . s)


runResolve :: ResolveState -> InstBuilderState -> Resolve a -> Either Error ((a, ResolveState), InstBuilderState)
runResolve resolveState instBuilderState f = do
    runExcept $ runStateT (runStateT (unResolve f) resolveState) instBuilderState


pushSymbolTable :: Resolve ()
pushSymbolTable = do
    modify $ \s -> s { symTab = Map.empty : symTab s }


popSymbolTable :: Resolve ()
popSymbolTable = do
    modify $ \s -> s { symTab = tail (symTab s) }


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


lookm :: Symbol -> SymKey -> Resolve (Maybe Symbol)
lookm symbol key = do
    ress <- gets $ lookupSymTab symbol key . symTab
    case ress of
        [] -> return Nothing
        [x] -> return (Just x)
        _   -> fail ("multiple definitions for: " ++ prettySymbol symbol)
    

lookHeadm :: Symbol -> SymKey -> Resolve (Maybe Symbol)
lookHeadm symbol key = do
    symTabHead <- gets (head . symTab)
    case lookupSymTab symbol key [symTabHead] of
        [] -> return Nothing
        [x] -> return (Just x)
        _   -> fail ("multiple definitions for: " ++ prettySymbol symbol)


look :: Symbol -> SymKey -> Resolve Symbol
look symbol key = do
    resm <- lookm symbol key
    unless (isJust resm) $ do
        (fail $ "undefined symbol: " ++ prettySymbol symbol)
    return (fromJust resm)


define :: Symbol -> SymKey -> Symbol -> Bool -> Resolve ()
define symbol@(SymResolved _) key symbol2 isQualified = do
    --liftIO $ putStrLn $ "defining: " ++ prettySymbol symbol
    resm <- lookHeadm symbol key
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symTab = (Map.insert (symbol, key, isQualified) symbol2 $ head $ symTab s) : (tail $ symTab s) }


genGeneric :: Symbol -> Resolve Symbol
genGeneric symbol@(SymResolved str) = do
    modName <- gets modName
    resm <- gets (Map.lookup symbol . supply)
    let n = maybe 0 (id) resm
    modify $ \s -> s { supply = Map.insert symbol (n + 1) (supply s) }
    case n of
        0 -> return $ SymResolved $ [modName] ++ str
        n -> return $ SymResolved $ [modName] ++ str ++ [show n]

genSymbol :: Symbol -> Resolve Symbol
genSymbol symbol@(SymResolved str) = do
    modName <- gets modName
    resm <- gets (Map.lookup symbol . supply)
    let n = maybe 0 (id) resm
    modify $ \s -> s { supply = Map.insert symbol (n + 1) (supply s) }
    case n of
        0 -> return $ SymResolved $ [modName] ++ str
        n -> return $ SymResolved $ [modName] ++ str ++ [show n]


resolveAst :: AstBuilderState -> [(Import, ASTResolved)] ->
    Either Error (AstBuilderState, Map.Map Symbol Int)
resolveAst ast imports = do
    fmap (fst . fst) $ runResolve initResolveState initInstBuilderState (resolveAst' ast)
    where
        resolveAst' :: AstBuilderState -> Resolve (AstBuilderState, Map.Map Symbol Int)
        resolveAst' ast = do
            modify $ \s -> s { modName = AstBuilder.abModuleName ast }

            forM_ imports $ \(Import isExport isQualified path mName, imprt) -> do
                forM_ (typeDefsTop imprt) $ \symbol -> do
                    let SymResolved [mod, name] = symbol
                    symbol' <- case mName of
                        Nothing -> return symbol
                        Just n  -> return $ SymResolved [n, name]

                    define symbol' KeyType symbol isQualified

            pushSymbolTable

            -- pre-define top-level symbols
            topStmts' <- forM (topStmts ast) $ \stmt -> case stmt of
                TopStmt (Typedef pos generics (Sym str) typ) -> do
                    symbol' <- genSymbol (SymResolved str)
                    define symbol' KeyType symbol' False
                    return $ TopStmt (Typedef pos generics symbol' typ)

                TopStmt (Function pos generics funDeps symbol funcType) -> do
                    symbol' <- genSymbol (SymResolved $ symStr symbol)
                    define symbol' KeyType symbol' False
                    return $ TopStmt (Function pos generics funDeps symbol' funcType)

                TopStmt (Enum pos generics symbol fields) -> do
                    symbol' <- genSymbol (SymResolved $ symStr symbol)
                    define symbol' KeyType symbol' False

                    fieldSymbols' <- forM fields $ \(fieldSymbol, _) -> do
                        fieldSymbol' <- genSymbol (SymResolved $ symStr fieldSymbol)
                        define fieldSymbol' KeyType fieldSymbol' False
                        return fieldSymbol'

                    return $ TopStmt $ Enum pos generics symbol' $ zip fieldSymbols' (map snd fields)


                _ -> return stmt

            topStmts'' <- mapM resolveTopStmt topStmts'

            supply <- gets supply

            popSymbolTable
            return (ast { topStmts = topStmts'' }, supply)


defineGenerics :: [Symbol] -> Resolve [Symbol]
defineGenerics generics = forM generics $ \(Sym str) -> do
    symbol <- genGeneric $ SymResolved str
    define symbol KeyType symbol False
    return symbol


resolveParam :: Param -> Resolve Param
resolveParam param = withPos param $ case param of
    Param pos (Sym sym) (Apply Ref t) -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar symbol False
        RefParam pos symbol <$> resolveType t

    Param pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar symbol False
        Param pos symbol <$> resolveType typ
    RefParam pos (Sym sym) typ -> do
        symbol <- genSymbol (SymResolved sym)
        define symbol KeyVar symbol False
        RefParam pos symbol <$> resolveType typ


resolveRetty :: Retty -> Resolve Retty
resolveRetty retty = case retty of
    RefRetty typ -> RefRetty <$> resolveType typ
    Retty typ    -> Retty <$> resolveType typ


resolveType :: Type -> Resolve Type
resolveType typ = case typ of
    TypeDef s      -> case s of
        Sym ["U8"]    -> return U8
        Sym ["I8"]    -> return I8
        Sym ["I16"]   -> return I16
        Sym ["I32"]   -> return I32
        Sym ["I64"]   -> return I64
        Sym ["F32"]   -> return F32
        Sym ["F64"]   -> return F64
        Sym ["Char"]  -> return Type.Char
        Sym ["Bool"]  -> return Type.Bool
        Sym ["Array"] -> return Type.Array
        Sym ["Table"] -> return Table
        Sym ["Sum"]   -> return Sum
        Sym ["Tuple"] -> return Tuple
        Sym ["Slice"] -> return Slice
        Sym ["Func"]  -> return Type.Func
        _             -> TypeDef <$> look s KeyType

    Apply t1 t2 -> do
        t1' <- resolveType t1
        t2' <- resolveType t2
        return (Apply t1' t2')

    _ -> return typ


resolveTopStmt :: TopStmt -> Resolve TopStmt
resolveTopStmt statement = case statement of
    TopStmt (Function pos generics funDeps symbol funcType) -> do
        unless (symbolIsResolved symbol) (fail "feature symbol wasn't resolved")
        pushSymbolTable
        generics' <- defineGenerics generics
        funDeps' <- forM funDeps $ \(a, b) -> do
            let [a'] = filter (symbolsCouldMatch a) generics'
            let [b'] = filter (symbolsCouldMatch b) generics'
            return (a', b')
        funcType' <- resolveType funcType
        popSymbolTable
        return $ TopStmt (Function pos generics' funDeps' symbol funcType')


    TopInst pos generics typ args isRef instState -> do
        pushSymbolTable
        generics' <- defineGenerics generics
        typ' <- resolveType typ
        args' <- mapM resolveParam args

        instState' <- resolveInstState instState

        popSymbolTable
        return $ TopInst pos generics' typ' args' isRef instState'


    TopStmt (Typedef pos generics symbol typ) -> do
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
        return $ TopStmt (Typedef pos generics' symbol' typ')

    TopStmt (Derives pos generics t1 ts) -> do
        pushSymbolTable
        generics' <- defineGenerics generics
        t1' <- resolveType t1
        ts' <- mapM resolveType ts
        popSymbolTable
        return $ TopStmt (Derives pos generics' t1' ts')

    TopStmt (Enum pos generics symbol fields) -> do
        symbol' <- case symbol of
            SymResolved _ -> return symbol

        pushSymbolTable
        generics' <- defineGenerics generics
        fieldTypes' <- forM fields $ \(_, fieldTypes) -> mapM resolveType fieldTypes
        popSymbolTable
        fieldSymbols' <- forM fields $ \(fieldSymbol, _) -> case fieldSymbol of
            SymResolved _ -> return fieldSymbol

        return $ TopStmt $ Enum pos generics' symbol' (zip fieldSymbols' fieldTypes')

    TopStmt x -> error (show x)


resolveInstState :: InstBuilderState -> Resolve InstBuilderState
resolveInstState instState = do
    liftInstBuilderState $ modify $ \s -> initInstBuilderState
    let Just (Block stmts) = Map.lookup 0 (statements instState)
    withCurId 0 $ mapM (resolveStmt instState) stmts
    liftInstBuilderState $ get


resolveBlock :: InstBuilderState -> Stmt -> Resolve ID
resolveBlock instState (Stmt id) = do
    let Just stmt = Map.lookup id (statements instState)
    case stmt of
        Block stmts -> do
            pushSymbolTable
            id <- newStmt (Block [])
            withCurId id $ mapM (resolveStmt instState) stmts
            popSymbolTable
            return id


resolveStmt :: InstBuilderState -> Stmt -> Resolve ()
resolveStmt instState (Stmt id) = do
    let Just statement = Map.lookup id (statements instState)
    case statement of
        Block stmts -> do
            pushSymbolTable
            id <- appendStmt (Block [])
            withCurId id $ mapM_ (resolveStmt instState) stmts
            popSymbolTable
            
        Return pos mexpr -> void $ appendStmt . Return pos =<< traverse (resolveExpr instState) mexpr
        ExprStmt expr -> void $ appendStmt . ExprStmt =<< resolveExpr instState expr
        EmbedC pos [] str -> do
            strMap <- processCEmbed str
            void $ appendStmt $ EmbedC pos strMap str

        Let pos pattern mexpr Nothing -> do
            pattern' <- resolvePattern instState pattern
            mexpr'   <- traverse (resolveExpr instState) mexpr
            void $ appendStmt $ Let pos pattern' mexpr' Nothing

        With pos exprs blk -> do
            exprs' <- mapM (resolveExpr instState) exprs
            id <- resolveBlock instState blk
            void $ appendStmt $ With pos exprs' (Stmt id)

        If pos expr stmt melse -> do
            pushSymbolTable
            expr' <- resolveExpr instState expr
            trueId <- resolveBlock instState stmt
            popSymbolTable
            pushSymbolTable

            falseId <- traverse (resolveBlock instState) melse
            popSymbolTable
            void $ appendStmt $ If pos expr' (Stmt trueId) (fmap Stmt falseId)

        While pos expr blk -> do
            pushSymbolTable
            expr' <- resolveExpr instState expr
            id <- resolveBlock instState blk
            popSymbolTable
            void $ appendStmt $ While pos expr' (Stmt id)

        For pos expr mpat blk -> do
            pushSymbolTable
            expr' <- resolveExpr instState expr
            mpat' <- traverse (resolvePattern instState) mpat
            id <- resolveBlock instState blk
            popSymbolTable
            void $ appendStmt $ For pos expr' mpat' (Stmt id)

        Switch pos expr cases -> do
            pushSymbolTable
            expr' <- resolveExpr instState expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pushSymbolTable
                pat' <- resolvePattern instState pat
                id <- resolveBlock instState stmt
                popSymbolTable
                return (pat', Stmt id)
            popSymbolTable
            void $ appendStmt $ Switch pos expr' cases'

        x -> error (show x)


resolvePattern :: InstBuilderState -> Pattern -> Resolve Pattern
resolvePattern instState pattern = withPos pattern $ fmap Pattern $ case pattern of
    PatAnnotated pat typ -> do
        Pattern id <- resolvePattern instState pat
        newType id =<< resolveType typ
        return id

    _ -> do
        id <- newPattern =<< case pattern of
            PatIgnore pos         -> return (PatIgnore pos)
            PatTuple pos pats     -> PatTuple pos <$> mapM (resolvePattern instState) pats
            PatLiteral expr       -> PatLiteral <$> resolveExpr instState expr
            PatField pos symbol pat -> do
                symbol' <- look symbol KeyType
                PatField pos symbol' <$> resolvePattern instState pat

            PatGuarded pos pat expr -> do
                pat' <- resolvePattern instState pat
                PatGuarded pos pat' <$> resolveExpr instState expr

            PatIdent pos (Sym str) -> do
                symbol <- genSymbol (SymResolved str)
                define symbol KeyVar symbol False
                return (PatIdent pos symbol)

            x -> error (show x)

        newType id (Type 0)


resolveExpr :: InstBuilderState -> Expr -> Resolve Expr
resolveExpr state expression = withPos expression $ fmap Expr $ case expression of
    AExpr typ expr -> do
        expr'@(Expr id) <- resolveExpr state expr
        newType id =<< resolveType typ
        return id

    _ -> do 
        id <- newExpr =<< case expression of
            AST.String pos s -> return (AST.String pos s)
            AST.Array pos exprs -> AST.Array pos <$> mapM (resolveExpr state) exprs
            AST.Int pos n -> return (AST.Int pos n)
            AST.Bool pos b -> return (AST.Bool pos b)
            AST.Char pos c -> return (AST.Char pos c)
            AST.Float pos f -> return (AST.Float pos f)
            Ident pos symbol -> Ident pos <$> look symbol KeyVar
            Reference pos expr -> Reference pos <$> resolveExpr state expr

            Call pos typ exprs -> do
                id <- generateId
                newType id =<< resolveType typ
                Call pos (Type id) <$> mapM (resolveExpr state) exprs

            Match pos expr pat -> do
                expr' <- (resolveExpr state) expr
                Match pos expr' <$> resolvePattern state pat

            x -> error (show x)

        newType id (Type 0)
                    

processCEmbed :: String -> Resolve [(String, Symbol)]
processCEmbed ('$':xs) = do
    let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
    check (length ident > 0)     "invalid identifier following '$' token"
    check (isAlpha $ ident !! 0) "invalid identifier following '$' token"
    let rest = drop (length ident) xs
    symbol <- look (Sym [ident]) KeyVar
    ((ident, symbol) :) <$> processCEmbed rest

processCEmbed (x:xs) = processCEmbed xs
processCEmbed [] = return []
