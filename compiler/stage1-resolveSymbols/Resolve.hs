module Resolve where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Char

import qualified SymTab
import Type
import AST
import Monad
import Error
import Symbol
import ASTResolved
import ASTMapper

-- Modoule 'Resolve':
--
-- 1.) Updates local symbols to be scope-agnostic: x -> x_0
-- 2.) Updates imported symbols to contain module: x -> mod_x_0
--
-- function calls will not be changed because the exact definiton of these symbols cannot be 
-- determined at this stage.

class Resolve a where
    resolve :: a -> DoM ResolveState a

data SymKey
    = KeyType
    | KeyVar
    deriving (Show, Eq, Ord)


type SymTab = SymTab.SymTab String SymKey Symbol


-- TODO
-- currently funcDefs contains all defined functions and all local functions.
-- Need to sort out the categories of function maps, probably need a funcDefsAll.
--
-- Also, lookFuncSymbol is good and it needs to work fully and provide lists of symbols to
-- calling points.


initAstResolved modName imports = ASTResolved
    { moduleName = modName
    , includes       = Set.empty
    , links          = Set.empty
    , featuresAll    = Map.unions (map featuresAll imports)
    , featuresTop    = Set.empty
    , typeDefsAll    = Map.unions (map typeDefsAll imports)
    , typeDefs       = Set.empty
    , symSupply      = Map.empty
    , funcDefsAll    = Map.unions (map funcDefsAll imports)
    , funcDefsTop    = Set.empty
    , funcInstance   = Map.empty
    , funcInstanceImported = Map.unions $
        (map funcInstance imports) ++ (map funcInstanceImported imports)
    }


modifyAst :: (ASTResolved -> ASTResolved) -> DoM ResolveState ()
modifyAst f = modify $ \s -> s { ast = f (ast s) }


data ResolveState
    = ResolveState
        { symTab           :: SymTab
        , ast              :: ASTResolved
        , typeDefsImported :: Set.Set Symbol
        , typeDefsLocal    :: SymTab.SymTab String () Symbol
        , funcDefsImported :: Set.Set Symbol
        , funcDefsLocal    :: SymTab.SymTab String () [Symbol]
        , featuresImported :: Set.Set Symbol
        , featuresLocal    :: SymTab.SymTab String () [Symbol]
        }

initResolveState imports modName = ResolveState
    { symTab           = SymTab.initSymTab
    , ast              = initAstResolved modName imports
    , typeDefsImported = Set.unions (map typeDefs imports)
    , funcDefsImported = Set.unions (map funcDefsTop imports)
    , featuresImported = Set.unions (map featuresTop imports)
    , typeDefsLocal    = SymTab.initSymTab
    , funcDefsLocal    = SymTab.initSymTab
    , featuresLocal    = SymTab.initSymTab
    }


defineFuncSymbol :: String -> Symbol -> DoM ResolveState ()
defineFuncSymbol str symbol = do
    resm <- SymTab.lookupHead str () <$> gets funcDefsLocal
    current <- case resm of
        Nothing -> return []
        Just xs -> return xs
    modify $ \s -> s { funcDefsLocal = SymTab.insert str () (symbol : current) (funcDefsLocal s) }


defineFeatureSymbol :: String -> Symbol -> DoM ResolveState ()
defineFeatureSymbol str symbol = do
    resm <- SymTab.lookupHead str () <$> gets featuresLocal
    current <- case resm of
        Nothing -> return []
        Just xs -> return xs
    modify $ \s -> s { featuresLocal = SymTab.insert str () (symbol : current) (featuresLocal s) }


lookFuncSymbol :: String -> DoM ResolveState [Symbol]
lookFuncSymbol str = do
    locals <- concat . map snd . SymTab.lookupAll str <$> gets funcDefsLocal
    imported <- Set.toList . Set.filter (\s -> Symbol.sym s == str) <$> gets funcDefsImported
    features <- lookFeature str
    return (locals ++ imported ++ features)


lookFeature :: String -> DoM ResolveState [Symbol]
lookFeature str = do
    featuresLocal <- concat . map snd . SymTab.lookupAll str <$> gets featuresLocal
    featuresImported <- Set.toList . Set.filter (\s -> Symbol.sym s == str) <$> gets featuresImported
    return (featuresLocal ++ featuresImported)


look :: Symbol -> SymKey -> DoM ResolveState Symbol
look symbol key = do
    lm <- lookm symbol key
    check (isJust lm) $ prettySymbol symbol ++ " isn't defined"
    return $ fromJust lm


lookm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookm (Sym sym) KeyVar = SymTab.lookup sym KeyVar <$> gets symTab
lookm symbol KeyType = do
    results <- case symbol of
        Sym sym -> do
            resm <- SymTab.lookup sym () <$> gets typeDefsLocal
            case resm of
                Just s -> return [s]
                Nothing -> do
                    typeFuncs <- gets typeDefsImported
                    modName <- gets (moduleName . ast)
                    return $ Set.toList $ Set.filter
                            (\s -> Symbol.sym s == sym && modName /= Symbol.mod s)
                            typeFuncs

        SymQualified mod sym -> do
            modName <- gets (moduleName . ast)
            if mod == modName then do
                resm <- SymTab.lookup sym () <$> gets typeDefsLocal
                case resm of
                    Just x -> return [x]
                    Nothing -> return []
            else do
                typeFuncs <- gets typeDefsImported
                return $ Set.toList $ Set.filter
                        (\s -> symbolsCouldMatch symbol s)
                        typeFuncs

        _ -> fail $ prettySymbol symbol

    case results of
        [] -> return Nothing
        [x] -> return (Just x)
        x   -> fail $ "ambiguous symbol: " ++ prettySymbol symbol ++ ", use qualifier."


genSymbol :: String -> DoM ResolveState Symbol
genSymbol sym = do  
    modName <- gets (moduleName . ast)
    im <- gets $ Map.lookup sym . symSupply . ast
    let n = maybe 0 (id) im
    modifyAst $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return $ SymResolved (modName ++ "::" ++ sym) n
        

define :: String -> SymKey -> Symbol -> DoM ResolveState ()
define sym key symbol = do
    resm <- gets $ SymTab.lookupHead sym key . symTab
    check (isNothing resm) $ sym ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key symbol (symTab s) }


pushSymbolTable :: DoM ResolveState ()
pushSymbolTable = do
    modify $ \s -> s
        { symTab        = SymTab.push (symTab s)
        , typeDefsLocal = SymTab.push (typeDefsLocal s)
        , funcDefsLocal = SymTab.push (funcDefsLocal s)
        }


popSymbolTable :: DoM ResolveState ()
popSymbolTable = do
    modify $ \s -> s
        { symTab = SymTab.pop (symTab s)
        , typeDefsLocal = SymTab.pop (typeDefsLocal s)
        , funcDefsLocal = SymTab.pop (funcDefsLocal s)
        }


resolveAsts :: [AST] -> [ASTResolved] -> DoM s (ASTResolved, ResolveState)
resolveAsts asts imports = runDoMExcept (initResolveState imports (astModuleName $ head asts)) $
    withErrorPrefix "symbol resolver: " $ do
        let includes = [ s | inc@(CInclude s) <- concat $ map astImports asts ]
        let links    = [ s | link@(CLink s) <- concat $ map astImports asts ]
        let typedefs = [ stmt | stmt@(AST.Typedef _ _ _ _) <- concat $ map astStmts asts ]
        let funcdefs = [ stmt | stmt@(AST.FuncDef _) <- concat $ map astStmts asts ]
        let features = [ stmt | stmt@(AST.Feature _ _ _) <- concat $ map astStmts asts ]

        modifyAst $ \s -> s { includes = Set.fromList includes , links = Set.fromList links }


        forM_ typedefs $ \(Typedef pos generics (Sym str) _) -> do
            symbol' <- genSymbol str
            modify $ \s -> s { typeDefsLocal = SymTab.insert str () symbol' (typeDefsLocal s) }

        -- give all funcdefs a unique name and define symbols.
        funcdefs' <- forM funcdefs $ \(FuncDef (Func header@(FuncHeader _ _ (Sym str) _ _) stmt)) -> do
            symbol' <- genSymbol str
            defineFuncSymbol str symbol'
            return $ FuncDef (Func header{funcSymbol = symbol'} stmt)


        -- define all feature funcs
        -- TODO currently it just defines some function symbols for the resolver to look at
        forM_ features $ \(Feature pos (Sym fstr) headers) -> do
            featureSymbol <- genSymbol fstr

            forM_ headers $ \(FuncHeader pos generics (Sym str) args retty)  -> do
                symbol' <- genSymbol (fstr ++ "_" ++ str)
                defineFeatureSymbol str symbol'

                pushSymbolTable
                genericSymbols <- defineGenerics generics
                args' <- mapM resolve args
                retty' <- resolve retty
                popSymbolTable

                let header' = (FuncHeader pos genericSymbols symbol' args' retty')

                modifyAst $ \s -> s { featuresTop = Set.insert symbol' (featuresTop s) }
                modifyAst $ \s -> s { featuresAll = Map.insert symbol' header' (featuresAll s) }


        -- check validity
        unless (all (== (astModuleName $ head asts)) $ map astModuleName asts)
            (error "module name mismatch")
        forM_ (concat $ map astStmts asts) $ \stmt -> withPos stmt $ case stmt of
            (AST.Typedef _ _ _ _) -> return ()
            (AST.FuncDef _) -> return ()
            (AST.Feature _ _ _) -> return ()
            _ -> fail "invalid top-level statement"


        -- top-level module type defs
        forM_ typedefs $ \typedef -> do
            (symbol, generics, typ) <- resolveTypeDef typedef
            modify $ \s -> s { typeDefsImported = Set.insert symbol (typeDefsImported s) }
            modifyAst $ \s -> s { typeDefsAll = Map.insert
                    symbol
                    (generics, typ)
                    (typeDefsAll s) }
            modifyAst $ \s -> s { typeDefs = Set.insert symbol (typeDefs s) }


        forM_ funcdefs' $ \funcdef@(FuncDef (Func header _))-> do
            func' <- resolveFuncDef funcdef
            let defSymbol = funcSymbol (funcHeader func')


            case Symbol.sym defSymbol of
                _ -> do
                    when (not $ isGenericFunc func') $ do
                        instanceSymbol <- genSymbol ("instance_" ++ Symbol.sym defSymbol)
                        modifyAst $ \s -> s { funcInstance = Map.insert
                            (callHeaderFromFuncHeader $ funcHeader func')
                            (func' {funcHeader = (funcHeader func') {funcSymbol = instanceSymbol}})
                            (funcInstance s) }

                    modifyAst $ \s -> s
                        { funcDefsTop = Set.insert defSymbol (funcDefsTop s)
                        , funcDefsAll = Map.insert defSymbol func' (funcDefsAll s)
                        }

        popSymbolTable

        gets ast


defineGenerics :: [Symbol] -> DoM ResolveState [Symbol]
defineGenerics generics = forM generics $ \(Sym str) -> do
    symbol <- genSymbol ("<generic>" ++ str)

    modify $ \s -> s { typeDefsLocal = SymTab.insert str () symbol (typeDefsLocal s) }
    return symbol


resolveFuncDef :: AST.Stmt -> DoM ResolveState Func
resolveFuncDef (FuncDef (Func (FuncHeader pos generics symbol args retty) blk)) = withPos pos $ do
    pushSymbolTable

    genericSymbols <- defineGenerics generics
    args' <- mapM resolve args
    retty' <- resolve retty

    blk' <- resolve blk

    when (Symbol.sym symbol == "main") $ do
        check (generics == []) "main cannot be generic"
        check (args     == []) "main cannot have arguments"
        check (retty == AST.Retty Void)  "main cannot have a return type"

    popSymbolTable

    return $ Func
        { funcHeader = (FuncHeader
            { funcGenerics = genericSymbols
            , funcArgs     = args'
            , funcRetty    = retty'
            , funcSymbol   = symbol
            , funcPos      = pos
            })
        , funcStmt  = blk'
        }


resolveTypeDef :: AST.Stmt -> DoM ResolveState (Symbol, [Symbol], Type)
resolveTypeDef (AST.Typedef pos generics (Sym sym) typ) = withPos pos $ do
    symbol <- look (Sym sym) KeyType

    -- Push the symbol table in order to temporarily define the type argument as a typedef
    pushSymbolTable
    genericSymbols <- defineGenerics generics
    typ' <- resolve typ
    popSymbolTable

    return (symbol, genericSymbols, typ')


instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        ExprStmt callExpr -> ExprStmt <$> resolve callExpr
        EmbedC pos str -> EmbedC pos <$> processCEmbed str

        FuncDef (Func header@(FuncHeader pos generics (Sym sym) args retty) blk) -> do
            symbol <- genSymbol sym
            defineFuncSymbol sym symbol

            func' <- resolveFuncDef $ FuncDef (Func header{funcSymbol = symbol} blk)

            let header' = (funcHeader func')
            let callHeader = callHeaderFromFuncHeader header'
            when (not $ isGenericHeader header') $ do
                instanceSymbol <- genSymbol ("instance_" ++ sym)
                modifyAst $ \s -> s { funcInstance = Map.insert
                    callHeader
                    (func' {funcHeader = header' {funcSymbol = instanceSymbol}})
                    (funcInstance s) }

            modifyAst $ \s -> s { funcDefsAll = Map.insert symbol func' (funcDefsAll s) }

            return $ FuncDef $ Func header' (funcStmt func')


        AST.Typedef pos args (Sym str) anno -> do
            symbol' <- genSymbol str
            modify $ \s -> s { typeDefsLocal = SymTab.insert str () symbol' (typeDefsLocal s) }
            (_, generics, typ) <- resolveTypeDef stmt
            modifyAst $ \s -> s { typeDefsAll = Map.insert symbol' (generics, typ) (typeDefsAll s) }

            return $ AST.Typedef pos args (Sym str) anno -- essentially discarded

        Block stmts -> do
            pushSymbolTable
            stmts' <- mapM resolve stmts

            -- filter out statements
            stmts'' <- fmap catMaybes $ forM stmts' $ \st -> case st of
                Typedef _ _ _ _ -> return Nothing
                FuncDef _ -> return Nothing
                _ -> return (Just st)

            popSymbolTable
            return $ Block stmts''

        Return pos mexpr -> case mexpr of
            Nothing -> return stmt
            Just expr -> Return pos . Just <$> resolve expr

        Let pos pat mexpr mblk -> do
            when (isJust mblk) pushSymbolTable
            mexpr' <- traverse resolve mexpr 
            pat' <- resolve pat
            mblk' <- traverse resolve mblk
            when (isJust mblk) popSymbolTable
            return $ Let pos pat' mexpr' mblk'
        
        If pos condition stmt melse -> do
            pushSymbolTable
            condition' <- resolve condition
            stmt' <- resolve stmt
            popSymbolTable
            pushSymbolTable
            melse' <- traverse resolve melse
            popSymbolTable
            return $ If pos condition' stmt' melse'

        While pos condition stmt -> do
            pushSymbolTable
            condition' <- resolve condition
            stmt' <- resolve stmt
            popSymbolTable
            return $ While pos condition' stmt' 

        Switch pos expr cases -> do
            expr' <- resolve expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pushSymbolTable
                pat' <- resolve pat
                stmt' <- resolve stmt
                popSymbolTable
                return (pat', stmt')
            return $ Switch pos expr' cases'
        
        For pos expr mpattern blk -> do
            pushSymbolTable
            expr' <- resolve expr
            mpattern' <- traverse resolve mpattern
            blk' <- resolve blk
            popSymbolTable
            return $ For pos expr' mpattern' blk'

        Data pos (Sym sym) typ mexpr -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
            typ' <- resolve typ
            mexpr' <- traverse resolve mexpr
            return $ Data pos symbol typ' mexpr'
        where
            processCEmbed :: String -> DoM ResolveState String
            processCEmbed ('$':xs) = do
                let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
                check (length ident > 0)     "invalid identifier following '$' token"
                check (isAlpha $ ident !! 0) "invalid identifier following '$' token"
                let rest = drop (length ident) xs

                symbol <- look (Sym ident) KeyVar
                (showSymLocal symbol ++) <$> processCEmbed rest
            processCEmbed (x:xs) = (x:) <$> processCEmbed xs
            processCEmbed [] = return ""


instance Resolve Type where resolve = mapTypeM resolveMapper
instance Resolve Pattern where resolve = mapPattern resolveMapper
instance Resolve Expr where resolve = mapExprM resolveMapper


instance Resolve Retty where
    resolve (Retty t) = Retty <$> resolve t
    resolve (RefRetty t) = RefRetty <$> resolve t

instance Resolve Param where
    resolve (Param pos (Sym sym) typ) = withPos pos $ do
        typ' <- resolve typ
        symbol <- genSymbol sym
        define sym KeyVar symbol
        return $ Param pos symbol typ'
    resolve (RefParam pos (Sym sym) typ) = withPos pos $ do
        typ' <- resolve typ
        symbol <- genSymbol sym
        define sym KeyVar symbol
        return $ RefParam pos symbol typ'

resolveMapper :: Elem -> DoM ResolveState Elem
resolveMapper element = case element of
    ElemExpr (Ident pos symbol) -> ElemExpr . Ident pos <$> look symbol KeyVar

    ElemType (Type.TypeApply (Symbol.Sym "Sum") ts)   -> return element
    ElemType (Type.TypeApply (Symbol.Sym "Tuple") ts) -> return element
    ElemType (Type.TypeApply (Symbol.Sym "Array") ts) -> return element
    ElemType (Type.TypeApply (Symbol.Sym "Table") ts) -> do
        let [t] = ts
        return element

    ElemType (Type.TypeApply s ts) -> do
        s' <- look s KeyType
        return $ ElemType (Type.TypeApply s' ts)

    ElemPatternIsolated (PatIdent pos (Sym sym)) -> do
        symbol' <- genSymbol sym
        define sym KeyVar symbol'
        return $ ElemPatternIsolated (PatIdent pos symbol')

    ElemPattern (PatIdent pos (Sym sym)) -> do
        symbol' <- genSymbol sym
        define sym KeyVar symbol'
        return $ ElemPattern (PatIdent pos symbol')

    ElemExpr (Call pos (Sym sym) exprs) -> do
        let list = [ "builtin_table_append"
                   , "builtin_table_at"
                   , "builtin_table_slice"
                   , "builtin_slice_at"
                   , "builtin_array_at"
                   , "conv"
                   , "assert"
                   ]
        if sym `elem` list then do
            return $ ElemExpr (Builtin pos sym exprs)
        else do
            symbols <- lookFuncSymbol sym
            case symbols of
                [] -> do
                    modname <- gets (moduleName . ast)
                    liftIO $ putStrLn $ (modname ++ " warning: no defs for: ") ++ show sym
                _ -> return ()
            
            return $ ElemExpr (Call pos (Sym sym) exprs)

    _ -> return element


