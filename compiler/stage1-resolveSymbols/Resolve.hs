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


initAstResolved modName = ASTResolved
    { moduleName = modName
    , includes   = Set.empty
    , links     = Set.empty
    , funcImports = Map.empty
    , funcExterns = Map.empty
    , funcDefs    = Map.empty
    , funcInstances = Map.empty
    , typeFuncs = Map.empty
    , typeFuncsVisible = Map.empty
    , symSupply = Map.empty
    }


modifyAst :: (ASTResolved -> ASTResolved) -> DoM ResolveState ()
modifyAst f = modify $ \s -> s { ast = f (ast s) }


data ResolveState
    = ResolveState
        { symTab       :: SymTab
        , ast          :: ASTResolved
        }

initResolveState imports modName = ResolveState
    { symTab = SymTab.initSymTab
    , ast    = initAstResolved modName
    }



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
            resm <- SymTab.lookup sym KeyType <$> gets symTab
            case resm of
                Just s -> return [s]
                Nothing -> do
                    typeFuncs <- gets (typeFuncsVisible . ast)
                    modName <- gets (moduleName . ast)
                    return $ Map.keys $ Map.filterWithKey
                            (\s _ -> Symbol.sym s == sym && modName /= Symbol.mod s)
                            typeFuncs

        SymQualified mod sym -> do
            modName <- gets (moduleName . ast)
            if mod == modName then do
                resm <- SymTab.lookup sym KeyType <$> gets symTab
                case resm of
                    Just x -> return [x]
                    Nothing -> return []
            else do
                typeFuncs <- gets (typeFuncsVisible . ast)
                return $ Map.keys $ Map.filterWithKey
                        (\s _ -> symbolsCouldMatch symbol s)
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
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymbolTable :: DoM ResolveState ()
popSymbolTable = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


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
        let includes   = [ s | inc@(CInclude s) <- concat $ map astImports asts ]
        let links      = [ s | link@(CLink s) <- concat $ map astImports asts ]
        let typedefs   = [ stmt | stmt@(AST.Typedef _ _ _ _) <- concat $ map astStmts asts ]
        let funcdefs   = [ stmt | stmt@(AST.FuncDef _) <- concat $ map astStmts asts ]

        typeFuncsVisible <- fmap Map.unions $ forM imports $ \imprt -> do
            return $ Map.filterWithKey
                (\s _ -> Symbol.mod s == moduleName imprt)
                (typeFuncsVisible imprt)
            

        modifyAst $ \s -> s
            { includes    = Set.fromList includes
            , links       = Set.fromList links
            , funcImports = Map.unions $ concat [map funcDefs imports]
            , funcExterns = Map.unions $ concat
                [ map ASTResolved.funcExterns imports
                , map ASTResolved.funcImports imports
                ]
            , typeFuncs = Map.unions (map typeFuncs imports)
            , typeFuncsVisible = typeFuncsVisible
            }

        -- check validity
        unless (all (== (astModuleName $ head asts)) $ map astModuleName asts)
            (error "module name mismatch")
        forM_ (concat $ map astStmts asts) $ \stmt -> withPos stmt $ case stmt of
            (AST.Typedef _ _ _ _) -> return ()
            (AST.FuncDef _) -> return ()
            _ -> fail "invalid top-level statement"

        mapM_ defineTypeSymbols typedefs
        mapM_ resolveTypeDef typedefs
        mapM_ resolveFuncDef funcdefs

        gets ast



resolveFuncDef :: AST.Stmt -> DoM ResolveState Symbol
resolveFuncDef (FuncDef (Func (FuncHeader pos generics (Sym sym) args retty) blk)) = withPos pos $ do
    symbol' <- genSymbol sym
    pushSymbolTable

    genericSymbols <- forM generics $ \(Sym s) -> do
        symbol <- (\s -> s { sym = ("<generic>" ++ Symbol.sym s) } ) <$> genSymbol s
        define s KeyType symbol
        return symbol

    args' <- mapM resolve args
    retty' <- resolve retty
    blk' <- resolve blk

    when (sym == "main") $ do
        check (generics == []) "main cannot be generic"
        check (args     == []) "main cannot have arguments"
        check (retty == AST.Retty Void)  "main cannot have a return type"

    let func = Func
            { funcHeader = (FuncHeader
                { funcGenerics = genericSymbols
                , funcArgs     = args'
                , funcRetty    = retty'
                , funcSymbol   = symbol'
                , funcPos      = pos
                })
            , funcStmt  = blk'
            }
    popSymbolTable
    modifyAst $ \s -> s { funcDefs = Map.insert symbol' func (funcDefs s) }
    return symbol'


defineTypeSymbols :: AST.Stmt -> DoM ResolveState ()
defineTypeSymbols (AST.Typedef pos _ (Sym sym) _) = withPos pos $ do
    symbol <- genSymbol sym
    define sym KeyType symbol


resolveTypeDef :: AST.Stmt -> DoM ResolveState ()
resolveTypeDef (AST.Typedef pos generics (Sym sym) anno) = withPos pos $ do
    symbol <- look (Sym sym) KeyType

    -- Push the symbol table in order to temporarily define the type argument as a typedef
    pushSymbolTable
    genericSymbols <- forM generics $ \(Sym s) -> do
        symbol <- (\s -> s { sym = ("<generic>" ++ (Symbol.sym s)) } ) <$> genSymbol s
        define s KeyType symbol
        return symbol

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
    modifyAst $ \s -> s { typeFuncsVisible = Map.insert symbol (genericSymbols, annoToType anno') (typeFuncsVisible s) }
    modifyAst $ \s -> s { typeFuncs = Map.insert symbol (genericSymbols, annoToType anno') (typeFuncs s) }
    where
        resolveTypedefParam :: AST.Param -> DoM ResolveState AST.Param
        resolveTypedefParam (AST.Param pos (Sym s) t) = withPos pos $ do
            s' <- genSymbol s
            AST.Param pos s' <$> resolve t



instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        ExprStmt callExpr -> ExprStmt <$> resolve callExpr
        EmbedC pos str -> EmbedC pos <$> processCEmbed str

        FuncDef (Func (FuncHeader pos generics (Sym sym) args retty) blk) -> do
            symbol' <- resolveFuncDef stmt
            func <- mapGet symbol' =<< gets (funcDefs . ast)
            return $ FuncDef $ Func ((funcHeader func) { funcSymbol = symbol' }) (funcStmt func)

        AST.Typedef pos args symbol anno -> do
            defineTypeSymbols stmt
            resolveTypeDef stmt
            return $ AST.Typedef pos args symbol anno -- essentially discarded

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
            return $ ElemExpr (Call pos (Sym sym) exprs)

    _ -> return element


