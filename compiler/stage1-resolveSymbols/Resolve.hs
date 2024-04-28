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

initAstResolved modName = ASTResolved
    { moduleName = modName
    , includes       = Set.empty
    , links          = Set.empty
    , funcImports    = Map.empty
    , funcDefs       = Map.empty
    , features       = Map.empty
    , funcInstances  = Map.empty
    , typeDefsAll    = Map.empty
    , typeDefs       = Set.empty
    , symSupply      = Map.empty
    }


modifyAst :: (ASTResolved -> ASTResolved) -> DoM ResolveState ()
modifyAst f = modify $ \s -> s { ast = f (ast s) }


data ResolveState
    = ResolveState
        { symTab           :: SymTab
        , ast              :: ASTResolved
        , typeDefsImported :: Set.Set Symbol
        , typeDefsLocal    :: SymTab.SymTab String () Symbol
        , funcDefsLocal    :: SymTab.SymTab String () [Symbol]
        }

initResolveState imports modName = ResolveState
    { symTab           = SymTab.initSymTab
    , ast              = initAstResolved modName
    , typeDefsImported = Set.empty
    , typeDefsLocal    = SymTab.initSymTab
    , funcDefsLocal    = SymTab.initSymTab
    }


defineFuncSymbol :: String -> Symbol -> DoM ResolveState ()
defineFuncSymbol str symbol = do
    resm <- SymTab.lookupHead str () <$> gets funcDefsLocal
    current <- case resm of
        Nothing -> return []
        Just xs -> return xs
    modify $ \s -> s { funcDefsLocal = SymTab.insert str () (symbol : current) (funcDefsLocal s) }


lookFuncSymbol :: String -> DoM ResolveState [Symbol]
lookFuncSymbol str = do
    locals <- concat . map snd . SymTab.lookupAll str <$> gets funcDefsLocal
    imported <- Map.keys . Map.filterWithKey (\s _ -> Symbol.sym s == str) <$>
        gets (funcImports . ast)
    return (locals ++ imported)


look :: Symbol -> SymKey -> DoM ResolveState Symbol
look symbol key = do
    lm <- lookm symbol key
    check (isJust lm) $ show symbol ++ " isn't defined"
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

        _ -> fail $ show symbol

    case results of
        [] -> return Nothing
        [x] -> return (Just x)
        x   -> fail $ "ambiguous symbol: " ++ show symbol ++ ", use qualifier."


genSymbol :: String -> DoM ResolveState Symbol
genSymbol sym = do  
    modName <- gets (moduleName . ast)
    im <- gets $ Map.lookup sym . symSupply . ast
    let n = maybe 0 (id) im
    modifyAst $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return (SymResolved modName sym n)
        

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


annoToType :: AnnoType -> Type
annoToType anno = case anno of
    AnnoTuple params  -> Type.TypeApply (Sym "Tuple") (map paramType params)
    AnnoApply s params -> Type.TypeApply s (map paramType params)
    AnnoTable params  -> error ""
    --AnnoSum  params   -> Type.Sum    (map paramType params)
    AnnoType t        -> t


resolveAsts :: [AST] -> [ASTResolved] -> DoM s (ASTResolved, ResolveState)
resolveAsts asts imports = runDoMExcept (initResolveState imports (astModuleName $ head asts)) $
    withErrorPrefix "symbol resolver: " $ do
        let includes = [ s | inc@(CInclude s) <- concat $ map astImports asts ]
        let links    = [ s | link@(CLink s) <- concat $ map astImports asts ]
        let typedefs = [ stmt | stmt@(AST.Typedef _ _ _ _) <- concat $ map astStmts asts ]
        let funcdefs = [ stmt | stmt@(AST.FuncDef _) <- concat $ map astStmts asts ]
        let features = [ stmt | stmt@(AST.Feature _ _ _ _) <- concat $ map astStmts asts ]

        modifyAst $ \s -> s
            { includes    = Set.fromList includes
            , links       = Set.fromList links
            , funcImports = Map.unions (map funcDefs imports)
            , typeDefsAll = Map.unions (map typeDefsAll imports)
            }


        -- define symbols. resolve uses maps for imports and symbols tables for 
        -- the current module.
        modify $ \s -> s { typeDefsImported = Set.unions (map typeDefs imports) }

        forM_ typedefs $ \(Typedef pos generics (Sym str) anno) -> do
            symbol' <- genSymbol str
            modify $ \s -> s { typeDefsLocal = SymTab.insert str () symbol' (typeDefsLocal s) }

        -- give all funcdefs a unique name and define symbols.
        funcdefs' <- forM funcdefs $ \(FuncDef (Func header@(FuncHeader _ _ (Sym str) _ _) stmt)) -> do
            symbol' <- genSymbol str
            defineFuncSymbol str symbol'
            return $ FuncDef (Func header{funcSymbol = symbol'} stmt)


        -- define all feature funcs
        -- TODO currently it just defines some function symbols for the resolver to look at
        features' <- forM features $ \(Feature pos generics (Sym str) headers) -> do
            pushSymbolTable
            genericSymbols <- defineGenerics generics

            headers' <- forM headers $ \(FuncHeader pos generics_ (Sym str) args retty)  -> do
                pushSymbolTable
                unless (generics_ == []) (fail "generics should be defined before feature")
                symbol' <- genSymbol str
                defineFuncSymbol str symbol'
                args' <- mapM resolve args
                retty' <- resolve retty
                popSymbolTable
                return $ FuncHeader pos [] symbol' args' retty'

            popSymbolTable

            return (Feature pos generics (Sym str) headers')

        -- check validity
        unless (all (== (astModuleName $ head asts)) $ map astModuleName asts)
            (error "module name mismatch")
        forM_ (concat $ map astStmts asts) $ \stmt -> withPos stmt $ case stmt of
            (AST.Typedef _ _ _ _) -> return ()
            (AST.FuncDef _) -> return ()
            (AST.Feature _ _ _ _) -> return ()
            _ -> fail "invalid top-level statement"


        -- top-level module type defs
        forM_ typedefs $ \typedef -> do
            (symbol, generics, anno) <- resolveTypeDef typedef
            modify $ \s -> s { typeDefsImported = Set.insert symbol (typeDefsImported s) }
            modifyAst $ \s -> s { typeDefsAll = Map.insert
                    symbol
                    (generics, annoToType anno)
                    (typeDefsAll s) }
            modifyAst $ \s -> s { typeDefs = Set.insert symbol (typeDefs s) }


        forM_ funcdefs' $ \funcdef@(FuncDef (Func header _))-> do
            func' <- resolveFuncDef funcdef
            modifyAst $ \s -> s { funcDefs = Map.insert (funcSymbol header) func' (funcDefs s) }

        popSymbolTable

        gets ast


defineGenerics :: [Symbol] -> DoM ResolveState [Symbol]
defineGenerics generics = forM generics $ \(Sym str) -> do
    symbol <- (\s -> s { sym = ("<generic>" ++ Symbol.sym s) } ) <$> genSymbol str
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



resolveTypeDef :: AST.Stmt -> DoM ResolveState (Symbol, [Symbol], AnnoType)
resolveTypeDef (AST.Typedef pos generics (Sym sym) anno) = withPos pos $ do
    symbol <- look (Sym sym) KeyType

    -- Push the symbol table in order to temporarily define the type argument as a typedef
    pushSymbolTable
    genericSymbols <- defineGenerics generics

    anno' <- case anno of
        AnnoType t        -> AnnoType <$> resolve t
        AnnoApply (Sym s) params -> do
            s' <- case s of
                "Sum" -> return (Sym s)
                "Table" -> return (Sym s)
                "Tuple" -> return (Sym s)
                _ -> genSymbol s
            params' <- forM params $ \param -> case param of
                Param pos (Sym s) t -> do
                    s' <- genSymbol s
                    t' <- resolve t
                    return (Param pos s' t')
            return (AnnoApply s' params')
            

        --AnnoSum params    -> AnnoSum    <$> mapM resolveTypedefParam params

    popSymbolTable

    return (symbol, genericSymbols, anno')



instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        ExprStmt callExpr -> ExprStmt <$> resolve callExpr
        EmbedC pos str -> EmbedC pos <$> processCEmbed str

        FuncDef (Func header@(FuncHeader pos generics (Sym sym) args retty) blk) -> do
            symbol <- genSymbol sym
            defineFuncSymbol sym symbol

            func' <- resolveFuncDef $ FuncDef (Func header{funcSymbol = symbol} blk)

            -- TODO push to local
            modifyAst $ \s -> s { funcDefs = Map.insert symbol func' (funcDefs s) }

            return $ FuncDef $ Func ((funcHeader func') { funcSymbol = symbol }) (funcStmt func')

        AST.Typedef pos args (Sym str) anno -> do
            symbol' <- genSymbol str
            modify $ \s -> s { typeDefsLocal = SymTab.insert str () symbol' (typeDefsLocal s) }
            (_, generics, anno) <- resolveTypeDef stmt
            modifyAst $ \s -> s { typeDefsAll = Map.insert symbol' (generics, annoToType anno) (typeDefsAll s) }

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
                (show symbol ++) <$> processCEmbed rest
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
                [] -> liftIO $ putStrLn $ ("warning: no defs for: ") ++ show sym
                _ -> return ()
            
            return $ ElemExpr (Call pos (Sym sym) exprs)

    _ -> return element


