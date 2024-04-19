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
    | KeyFunc
    | KeyVar
    deriving (Show, Eq, Ord)


type SymTab = SymTab.SymTab String SymKey Symbol

data ResolveState
    = ResolveState
        { symTab       :: SymTab
        , imports      :: [ASTResolved]
        , modName      :: String
        , supply       :: Map.Map String Int
        , funcDefsMap  :: Map.Map Symbol FuncBody
        , typeFuncsMap :: Map.Map Symbol ([Symbol], AnnoType)
        }

initResolveState imports modName typeImports = ResolveState
    { symTab        = SymTab.initSymTab
    , imports       = imports
    , modName       = modName
    , supply        = Map.empty
    , funcDefsMap   = Map.empty
    , typeFuncsMap  = Map.empty
    }



look :: Symbol -> SymKey -> DoM ResolveState Symbol
look symbol key = do
    lm <- lookm symbol key
    check (isJust lm) $ show symbol ++ " isn't defined"
    return $ fromJust lm


lookm :: Symbol -> SymKey -> DoM ResolveState (Maybe Symbol)
lookm (Sym sym) KeyVar = SymTab.lookup sym KeyVar <$> gets symTab
lookm symbol KeyFunc = case symbol of
    Sym sym -> do
        resm <- SymTab.lookup sym KeyFunc <$> gets symTab
        case resm of
            Just s -> return (Just s)
            Nothing -> return (Just symbol)

    SymQualified mod sym -> do
        modName <- gets modName
        if mod == modName then SymTab.lookup sym KeyFunc <$> gets symTab
        else lookm (Sym sym) KeyFunc

    _ -> fail $ show symbol

lookm symbol KeyType = case symbol of
    Sym sym -> do
        resm <- SymTab.lookup sym KeyType <$> gets symTab
        case resm of
            Just s -> return (Just s)
            Nothing -> do
                imprts <- gets imports
                xs <- fmap concat $ forM imprts $
                    \imprt -> return $ Map.keys $ Map.filterWithKey
                        (\s _ -> Symbol.sym s == sym && Symbol.mod s == moduleName imprt)
                        (typeFuncs imprt)

                case xs of
                    [] -> return Nothing
                    [x] -> return (Just x)
                    x   -> fail $ "ambiguous symbol: " ++ show symbol ++ ", use qualifier."

    SymQualified mod sym -> do
        modName <- gets modName
        if mod == modName then SymTab.lookup sym KeyType <$> gets symTab
        else do
            xs <- Set.toList . Set.fromList . concat . map (Map.keys . Map.filterWithKey (\s _ -> symbolsCouldMatch symbol s) . typeFuncs) <$> gets imports
            case xs of
                [] -> return Nothing
                [x] -> return (Just x)
                x   -> fail $ "ambiguous symbol: " ++ show symbol ++ ", use qualifier."

    _ -> fail $ show symbol


genSymbol :: String -> DoM ResolveState Symbol
genSymbol sym = do  
    modName <- gets modName
    im <- gets $ Map.lookup sym . supply
    let n = maybe 0 (id) im
    modify $ \s -> s { supply = Map.insert sym (n + 1) (supply s) }
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
    AnnoTable params  -> error ""
    --AnnoSum  params   -> Type.Sum    (map paramType params)
    AnnoType t        -> t


resolveAsts :: [AST] -> [ASTResolved] -> DoM s (ASTResolved, ResolveState)
resolveAsts asts imports = runDoMExcept (initResolveState imports (astModuleName $ head asts) Map.empty) $
    withErrorPrefix "symbol resolver: " $ do
        let moduleName = astModuleName (head asts)
        let includes   = [ s | inc@(CInclude s) <- concat $ map astImports asts ]
        let links      = [ s | link@(CLink s) <- concat $ map astImports asts ]
        let typedefs   = [ stmt | stmt@(AST.Typedef _ _ _ _) <- concat $ map astStmts asts ]
        let funcdefs   = [ stmt | stmt@(AST.FuncDef _ _ _ _ _ _ _) <- concat $ map astStmts asts ]

        -- check validity
        unless (all (== moduleName) $ map astModuleName asts) (error "module name mismatch")
        forM_ (concat $ map astStmts asts) $ \stmt -> withPos stmt $ case stmt of
            (AST.Typedef _ _ _ _) -> return ()
            (AST.FuncDef _ _ _ _ _ _ _) -> return ()
            _ -> fail "invalid top-level statement"

        -- get imports
        let typeFuncImportMap = Map.unions (map typeFuncs imports)

        mapM_ defineTypeSymbols typedefs
        mapM_ resolveTypeDef typedefs

        forM_ funcdefs $ \funcdef@(FuncDef _ generics params (Sym sym) args retty blk) -> do
            void $ resolveFuncDef funcdef

        typeFuncs <- gets typeFuncsMap
        funcDefs <- gets funcDefsMap

        supply <- gets supply
        return $ ASTResolved
            { moduleName  = moduleName
            , includes    = Set.fromList includes
            , links       = Set.fromList links
            , funcImports = Map.unions $ concat [map ASTResolved.funcDefs imports,  map ASTResolved.funcImports imports]
            , funcDefs    = funcDefs
            , typeFuncs   = Map.union typeFuncImportMap (Map.map (\(x, y) -> (x, annoToType y)) typeFuncs)
            , symSupply   = supply
            }



-- defines in funcDefsMap
resolveFuncDef :: AST.Stmt -> DoM ResolveState Symbol
resolveFuncDef (FuncDef pos generics params (Sym sym) args retty blk) = withPos pos $ do
    symbol' <- genSymbol sym
    pushSymbolTable
    genericSymbols <- mapM (\(Sym s) -> genSymbol s) generics
    forM_ genericSymbols $ \symbol -> define (Symbol.sym symbol) KeyType symbol
    params' <- mapM resolve params
    args' <- mapM resolve args
    retty' <- resolve retty
    blk' <- resolve blk

    when (sym == "main") $ do
        check (generics == []) "main cannot be generic"
        check (args     == []) "main cannot have arguments"
        check (retty == VoidRetty)  "main cannot have a return type"
        case params of
            []                                    -> return ()
            [Param _ _ (TypeApply (Sym "Io") [])] -> return ()
            _ -> check (False) "main may only have one Io parameter"

    let funcBody = FuncBody {
        funcGenerics = genericSymbols,
        funcParams   = params',
        funcArgs     = args',
        funcRetty    = retty',
        funcStmt     = blk'
        }
    popSymbolTable
    modify $ \s -> s { funcDefsMap = Map.insert symbol' funcBody (funcDefsMap s) }
    return symbol'


defineTypeSymbols :: AST.Stmt -> DoM ResolveState ()
defineTypeSymbols (AST.Typedef pos _ (Sym sym) _) = withPos pos $ do
    symbol <- genSymbol sym
    define sym KeyType symbol
    define sym KeyFunc symbol


-- modifies the typedef function and inserts it into typeFuncsMap
resolveTypeDef :: AST.Stmt -> DoM ResolveState ()
resolveTypeDef (AST.Typedef pos generics (Sym sym) anno) = withPos pos $ do
    symbol <- look (Sym sym) KeyType

    -- Push the symbol table in order to temporarily define the type argument as a typedef
    pushSymbolTable
    genericSymbols <- forM generics $ \(Sym arg) -> do
        s <- genSymbol arg
        define arg KeyType s
        return s

    anno' <- case anno of
        AnnoType t        -> AnnoType <$> resolve t
        AnnoTuple params  -> AnnoTuple  <$> mapM resolveTypedefParam params
        AnnoTable params  -> AnnoTable  <$> mapM resolveTypedefParam params
        --AnnoSum params    -> AnnoSum    <$> mapM resolveTypedefParam params

    popSymbolTable
    modify $ \s -> s { typeFuncsMap = Map.insert symbol (genericSymbols, anno') (typeFuncsMap s) }
    where
        resolveTypedefParam :: AST.Param -> DoM ResolveState AST.Param
        resolveTypedefParam (AST.Param pos (Sym s) t) = withPos pos $ do
            s' <- genSymbol s
            AST.Param pos s' <$> resolve t


instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        ExprStmt callExpr -> ExprStmt <$> resolve callExpr
        EmbedC pos str -> EmbedC pos <$> processCEmbed str

        FuncDef pos typeArgs params (Sym sym) args retty blk -> do
            symbol' <- resolveFuncDef stmt
            body <- mapGet symbol' =<< gets funcDefsMap
            return $ FuncDef
                pos
                (funcGenerics body)
                (funcParams body)
                symbol'
                (funcArgs body)
                (funcRetty body)
                (funcStmt body)

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
                FuncDef _ _ _ _ _ _ _ -> return Nothing
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

        SetOp pos op index expr -> do
            index' <- resolve index
            expr' <- resolve expr
            return $ SetOp pos op index' expr'

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
    resolve VoidRetty = return VoidRetty
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
    ElemType (Type.TypeApply (Symbol.Sym "Table") ts) -> do
        let [t] = ts
        return element

    ElemType (Type.TypeApply s ts) -> do
        s' <- look s KeyType
        return $ ElemType (Type.TypeApply s' ts)

    ElemPattern (PatIdent pos (Sym sym)) -> do
        symbol' <- genSymbol sym
        define sym KeyVar symbol'
        return $ ElemPattern (PatIdent pos symbol')

    ElemPattern (PatField pos symbol pats) -> do 
        symbol' <- look symbol KeyFunc
        return $ ElemPattern (PatField pos symbol' pats)

    ElemExpr (Construct pos symbol exprs) -> do
        symbol' <- look symbol KeyFunc
        return $ ElemExpr (Construct pos symbol' exprs)

    ElemExpr (Call pos mparam (Sym sym) exprs)
        | sym `elem` ["builtin_table_append", "builtin_len", "builtin_table_at", "conv", "print", "assert"] -> do 
            check (isNothing mparam) "invalid builtin function call"
            return $ ElemExpr (Builtin pos sym exprs)

        | otherwise -> do
            symbol' <- look (Sym sym) KeyFunc
            return $ ElemExpr (Call pos mparam symbol' exprs)

    _ -> return element


