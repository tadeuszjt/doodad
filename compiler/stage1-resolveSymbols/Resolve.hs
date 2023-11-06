{-# LANGUAGE FlexibleContexts #-}
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
-- 3.) Replaces builtin function calls with specific: len -> builtin len
-- 4.) Replaces PatField with PatTypeField when symbol is a type
--
-- function calls and tuple members will not be changed because the exact definiton of these symbols cannot be 
-- determined at this stage.

class Resolve a where
    resolve :: BoM ResolveState m => a -> m a

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



look :: BoM ResolveState m => Symbol -> SymKey -> m Symbol
look symbol key = do
    lm <- lookm symbol key
    assert (isJust lm) $ show symbol ++ " isn't defined"
    return $ fromJust lm


lookm :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Symbol)
lookm (Sym sym) KeyVar = SymTab.lookup sym KeyVar <$> gets symTab
lookm symbol key = case symbol of
    Sym sym -> do
        resm <- SymTab.lookup sym key <$> gets symTab
        case resm of
            Just s -> return (Just s)
            Nothing -> do
                case key of
                    -- if the symbol is a constructor, resolve here
                    KeyFunc -> do
                        imprts <- gets imports
                        xs <- fmap concat $ forM imprts $
                            \imprt -> return $ Map.keys $ Map.filterWithKey
                                (\s _ -> Symbol.sym s == sym && Symbol.mod s == moduleName imprt)
                                (ctorDefs imprt)
                        case xs of 
                            [x] -> return (Just x)
                            [] -> return (Just symbol) -- Maybe remove this
                            xs -> error (show xs)
                    KeyType -> do
                        xs <- Set.toList . Set.fromList . concat . map (Map.keys . Map.filterWithKey (\s _ -> Symbol.sym s == sym) . typeFuncs) <$> gets imports
                        case xs of
                            [] -> return Nothing
                            [x] -> return (Just x)

    SymQualified mod sym -> do
        modName <- gets modName
        if mod == modName then SymTab.lookup sym key <$> gets symTab
        else lookm (Sym sym) key

    _ -> fail $ show (symbol, key)


genSymbol :: BoM ResolveState m => String -> m Symbol
genSymbol sym = do  
    modName <- gets modName
    im <- gets $ Map.lookup sym . supply
    let n = maybe 0 (id) im
    modify $ \s -> s { supply = Map.insert sym (n + 1) (supply s) }
    let symbol = SymResolved modName sym n
    return symbol
        

define :: BoM ResolveState m => String -> SymKey -> Symbol -> m ()
define sym key symbol = do
    resm <- gets $ SymTab.lookupHead sym key . symTab
    assert (isNothing resm) $ sym ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key symbol (symTab s) }


pushSymbolTable :: BoM ResolveState m => m ()
pushSymbolTable = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymbolTable :: BoM ResolveState m => m ()
popSymbolTable = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


annoToType :: AnnoType -> Type
annoToType anno = case anno of
    AnnoTuple params  -> Type.Tuple $ Type.Record (map paramType params)
    AnnoTable params  -> Type.Table $ Type.Record (map paramType params)
    AnnoADT  params   -> Type.ADT    (map paramType params)
    AnnoRecord params -> Type.Record (map paramType params)
    AnnoType t        -> t


buildCtorImportMap :: BoM (Map.Map Symbol (Symbol, Int)) m => [ASTResolved] -> m ()
buildCtorImportMap imports = do
    forM_ imports $ \imprt -> do
        forM_ (Map.toList $ ctorDefs imprt) $ \(symbol, (typeSymbol, i)) -> do
            when (Symbol.mod symbol == moduleName imprt) $ do
                modify $ Map.insert symbol (typeSymbol, i)

buildCtorMap :: BoM (Map.Map Symbol (Symbol, Int)) m => [(Symbol, AnnoType)] -> m ()
buildCtorMap list = do
    forM_ list $ \(symbol, anno) -> case anno of
        AnnoADT ps -> forM_ (zip ps [0..]) $ \(Param _ s t, i) -> 
            modify $ Map.insert s (symbol, i)
        AnnoTuple ps -> forM_ (zip ps [0..]) $ \(Param _ s t, i) -> 
            modify $ Map.insert s (symbol, i)
        AnnoTable ps -> forM_ (zip ps [0..]) $ \(Param _ s t, i) -> 
            modify $ Map.insert s (symbol, i)
        AnnoRecord params -> forM_ (zip params [0..]) $ \(Param _ s t, i) ->
            modify $ Map.insert s (symbol, i)
        AnnoType t -> return ()
        _ -> error (show anno)



resolveAsts :: BoM s m => [AST] -> [ASTResolved] -> m (ASTResolved, ResolveState)
resolveAsts asts imports = withErrorPrefix "resolve: " $
    runBoMTExcept (initResolveState imports (astModuleName $ head asts) Map.empty) $ do
        let moduleName = astModuleName $ head asts
        let includes   = [ s | inc@(CInclude s) <- concat $ map astImports asts ]
        let links      = [ s | link@(CLink s) <- concat $ map astImports asts ]
        let typedefs   = [ stmt | stmt@(AST.Typedef _ _ _ _) <- concat $ map astStmts asts ]
        let funcdefs   = [ stmt | stmt@(AST.FuncDef _ _ _ _ _ _ _) <- concat $ map astStmts asts ]
        let consts     = [ stmt | stmt@(AST.Const _ _ _) <- concat $ map astStmts asts ]

        -- check validity
        assert (all (== moduleName) $ map astModuleName asts) "module name mismatch"
        forM_ (concat $ map astStmts asts) $ \stmt -> withPos stmt $ case stmt of
            (AST.Typedef _ _ _ _) -> return ()
            (AST.FuncDef _ _ _ _ _ _ _) -> return ()
            (AST.Const _ _ _) -> return ()
            _ -> fail "invalid top-level statement"

        -- define constants
        constDefsList <- forM consts $ \(AST.Const pos (Sym sym) expr) -> withPos pos $ do
            symbol' <- genSymbol sym
            define sym KeyVar symbol'
            return (symbol', expr)

        -- get imports
        let typeFuncImportMap = Map.unions (map typeFuncs imports)

        (_, ctorImportMap)     <- runBoMTExcept Map.empty (buildCtorImportMap imports)

        mapM resolveTypeDef typedefs
        mapM_ resolveFuncDef funcdefs

        typeFuncs <- gets typeFuncsMap
        funcDefs <- gets funcDefsMap

        -- combine the typeDefs and typeFuncs map to build the ctorMap
        ctorMap <- fmap snd $ runBoMTExcept Map.empty $ 
            buildCtorMap $ Map.toList $ Map.map snd typeFuncs

        supply <- gets supply
        return $ ASTResolved
            { moduleName  = moduleName
            , includes    = Set.fromList includes
            , links       = Set.fromList links
            , funcImports = Map.unions (map ASTResolved.funcDefs imports)
            , constDefs   = Map.fromList constDefsList
            , funcDefs    = funcDefs
            , typeFuncs   = Map.union typeFuncImportMap (Map.map (\(x, y) -> (x, annoToType y)) typeFuncs)
            , ctorDefs    = Map.union ctorImportMap ctorMap
            , symSupply   = supply
            }



-- defines in funcDefsMap
resolveFuncDef :: BoM ResolveState m => AST.Stmt -> m Symbol
resolveFuncDef (FuncDef pos typeArgs params (Sym sym) args retty blk) = withPos pos $ do
    pushSymbolTable
    typeSymbols <- mapM (\(Sym s) -> genSymbol s) typeArgs
    forM_ typeSymbols $ \symbol -> define (Symbol.sym symbol) KeyType symbol
    params' <- mapM resolve params
    args' <- mapM resolve args
    retty' <- resolve retty
    blk' <- resolve blk
    popSymbolTable

    symbol' <- genSymbol sym
    let funcBody = FuncBody {
        funcTypeArgs = typeSymbols,
        funcParams   = params',
        funcArgs     = args',
        funcRetty    = retty',
        funcStmt     = blk'
        }
    modify $ \s -> s { funcDefsMap = Map.insert symbol' funcBody (funcDefsMap s) }
    return symbol'


-- modifies the typedef function and inserts it into typeFuncsMap
resolveTypeDef :: BoM ResolveState m => AST.Stmt -> m ()
resolveTypeDef (AST.Typedef pos typeArgs (Sym sym) anno) = do
    symbol <- genSymbol sym
    define sym KeyType symbol
    define sym KeyFunc symbol

    -- Here we push the symbol table in order to temporarily define the type argument as a typedef
    pushSymbolTable
    argSymbols <- forM typeArgs $ \(Sym arg) -> do
        s <- genSymbol arg
        define arg KeyType s
        return s

    anno' <- case anno of
        AnnoType t -> AnnoType <$> resolve t
        AnnoTuple ps -> do 
            ps' <- forM ps $ \(AST.Param pos (Sym s) t) -> do
                s' <- genSymbol s
                AST.Param pos s' <$> resolve t
            return $ AnnoTuple ps'

        AnnoRecord params -> fmap AnnoRecord $ forM params $ \(AST.Param pos (Sym s) t) -> do
            s' <- genSymbol s
            AST.Param pos s' <$> resolve t

        AnnoTable params -> fmap AnnoTable $ forM params $ \(AST.Param pos (Sym s) t) -> do
            s' <- genSymbol s
            AST.Param pos s' <$> resolve t

        AnnoADT params -> fmap AnnoADT $ forM params $ \(AST.Param pos (Sym s) t) -> do
            s' <- genSymbol s
            Param pos s' <$> resolve t

    popSymbolTable
    modify $ \s -> s { typeFuncsMap = Map.insert symbol (argSymbols, anno') (typeFuncsMap s) }


instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        Increment pos expr -> Increment pos <$> resolve expr
        ExprStmt callExpr -> ExprStmt <$> resolve callExpr
        EmbedC pos str -> EmbedC pos <$> processCEmbed str

        FuncDef pos typeArgs params (Sym sym) args retty blk -> do
            symbol' <- resolveFuncDef stmt
            body <- mapGet symbol' =<< gets funcDefsMap
            return $ FuncDef
                pos
                (funcTypeArgs body)
                (funcParams body)
                symbol'
                (funcArgs body)
                (funcRetty body)
                (funcStmt body)

        AST.Typedef pos args symbol anno -> do
            resolveTypeDef stmt
            return $ AST.Typedef pos args symbol anno -- essentially discarded

        Const pos (Sym s) expr -> do
            symbol' <- genSymbol s
            define s KeyVar symbol'
            Const pos symbol' <$> resolve expr

        Block stmts -> do
            pushSymbolTable
            stmts' <- mapM resolve stmts

            -- filter out statements
            stmts'' <- fmap catMaybes $ forM stmts' $ \st -> case st of
                Typedef _ _ _ _ -> return Nothing
                FuncDef _ _ _ _ _ _ _ -> return Nothing
                Const _ _ _ -> return Nothing
                _ -> return (Just st)

            popSymbolTable
            return $ Block stmts''

        Return pos mexpr -> case mexpr of
            Nothing -> return stmt
            Just expr -> Return pos . Just <$> resolve expr

        Assign pos pat expr -> do
            expr' <- resolve expr 
            pat' <- resolve pat
            return $ Assign pos pat' expr'
        
        If pos condition stmt melse -> do
            pushSymbolTable
            condition' <- resolve condition
            stmt' <- resolve stmt
            popSymbolTable
            pushSymbolTable
            melse' <- maybe (return Nothing) (fmap Just . resolve) melse
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
            mpattern' <- maybe (return Nothing) (fmap Just . resolve) mpattern
            blk' <- resolve blk
            popSymbolTable
            return $ For pos expr' mpattern' blk'

        Data pos (Sym sym) typ mexpr -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
            typ' <- resolve typ
            mexpr' <- maybe (return Nothing) (fmap Just . resolve) mexpr
            return $ Data pos symbol typ' mexpr'
        where
            processCEmbed :: BoM ResolveState m => String -> m String
            processCEmbed ('$':xs) = do
                let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
                assert (length ident > 0) "invalid ident"
                assert (isAlpha $ ident !! 0) "invalid ident"
                let rest = drop (length ident) xs

                symbol <- look (Sym ident) KeyVar
                (show symbol ++) <$> processCEmbed rest
            processCEmbed (x:xs) = (x:) <$> processCEmbed xs
            processCEmbed [] = return ""


instance Resolve Type where resolve = mapTypeM resolveMapper
instance Resolve Pattern where resolve = mapPattern resolveMapper
instance Resolve Expr where resolve = mapExprM resolveMapper
instance Resolve Param where
    resolve (Param pos (Sym sym) typ) = withPos pos $ do
        typ' <- resolve typ
        symbol <- genSymbol sym
        define sym KeyVar symbol
        return $ Param pos symbol typ'

resolveMapper :: BoM ResolveState m => Elem -> m (Maybe Elem)
resolveMapper element = case element of
    ElemType (Type.TypeApply s ts) -> do
        symbol' <- look s KeyType
        return $ Just $ ElemType $ Type.TypeApply symbol' ts

    ElemPattern (PatIdent pos symbol) -> do
        let Sym sym = symbol
        symbol' <- genSymbol sym
        define sym KeyVar symbol'
        return $ Just $ ElemPattern (PatIdent pos symbol')

    ElemPattern (PatField pos symbol pats) -> do -- it's KeyFunc for ctors, KeyType for other
        mtype <- lookm symbol KeyType
        case mtype of
--          Just symbol' -> do
--              unless (length pats == 1) $ error "TODO - make it handle more"
--              return $ PatTypeField pos (Type.TypeApply symbol' []) (head pats')
            Nothing -> do
                symbol' <- look symbol KeyFunc
                return $ Just $ ElemPattern $ PatField pos symbol' pats

    ElemExpr (Ident pos symbol) -> Just . ElemExpr . Ident pos <$> look symbol KeyVar

    ElemExpr (Call pos params symbol exprs) -> case symbol of
        Sym s | s `elem` ["len", "conv", "print"] -> do 
            return $ Just $ ElemExpr (Builtin pos params s exprs)
        _ -> do
            resm <- lookm symbol KeyType
            case resm of 
                Just symbol' -> do
                    error "conv"
--                    assert (params == []) "Convert cannot have params"
--                    return $ Just $ ElemExpr $ Conv pos (Type.TypeApply symbol' []) exprs -- TODO
                Nothing -> do
                    symbol' <- look symbol KeyFunc
                    return $ Just $ ElemExpr (Call pos params symbol' exprs)

    _ -> return (Just element)


