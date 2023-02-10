{-# LANGUAGE FlexibleContexts #-}
module Resolve where

import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Data.List

import qualified SymTab
import Type
import AST
import Monad
import Error
import Symbol
import States

-- Modoule 'Resolve':
--
-- 1.) Updates local symbols to be scope-agnostic: x -> x_0
-- 2.) Updates imported symbols to contain module: x -> mod_x_0
-- 3.) Replaces builtin function calls with specific: push -> builtin push
--
-- function calls and tuple members will not be changed because the exact definiton of these symbols cannot be 
-- determined at this stage.


class Resolve a where
    resolve :: BoM ResolveState m => a -> m a

data SymKey
    = KeyType
    | KeyFunc
    deriving (Show, Eq, Ord)


type SymTab = SymTab.SymTab String SymKey Symbol

data ResolveState
    = ResolveState
        { symTab      :: SymTab
        , symTabVar   :: SymTab.SymTab String () Symbol
        , imports     :: [IRGenState]
        , modName     :: String
        , supply      :: Map.Map String Int
        }

initResolveState imports modName typeImports = ResolveState
    { symTab    = SymTab.initSymTab
    , symTabVar = SymTab.initSymTab
    , imports   = imports
    , modName   = modName
    , supply    = Map.empty
    }


lookVar :: BoM ResolveState m => Symbol -> m Symbol
lookVar (Sym sym) = do
    lm <- SymTab.lookup sym () <$> gets symTabVar
    assert (isJust lm) $ show sym ++ " isn't defined"
    return $ fromJust lm


look :: BoM ResolveState m => Symbol -> SymKey -> m Symbol
look symbol key = do
    lm <- lookm symbol key
    assert (isJust lm) $ show symbol ++ " isn't defined"
    return $ fromJust lm


lookm :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Symbol)
lookm symbol key = case symbol of
    Sym sym -> do
        resm <- SymTab.lookup sym key <$> gets symTab
        case resm of
            Just s -> return (Just s)
            Nothing -> do
                case key of
                    KeyFunc -> return (Just symbol)
                    KeyType -> do
                        xs <- catMaybes . map (Map.lookup sym . irTypeMap) <$> gets imports
                        case xs of
                            [] -> fail $ "no type definitions for: " ++ sym
                            [x] -> return (Just x)

    SymQualified mod sym -> do
        modName <- gets modName
        symTab <- gets symTab
        if mod == modName then  return $ SymTab.lookup sym key symTab
        else if mod == "c" then return (Just symbol)
        else                    lookm (Sym sym) key

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

defineVar :: BoM ResolveState m => String -> Symbol -> m ()
defineVar sym symbol = do
    resm <- gets $ SymTab.lookupHead sym () . symTabVar
    assert (isNothing resm) $ sym ++ " already defined"
    modify $ \s -> s { symTabVar = SymTab.insert sym () symbol (symTabVar s) }


pushSymTab :: BoM ResolveState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    modify $ \s -> s { symTabVar = SymTab.push (symTabVar s) }


popSymTab :: BoM ResolveState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }
    modify $ \s -> s { symTabVar = SymTab.pop (symTabVar s) }


annoToType :: AnnoType -> Type
annoToType anno = case anno of
    AnnoEnum ss -> Enum
    AnnoTuple xs -> Type.Tuple $ map snd xs
    AnnoADT  xs -> Type.ADT $ map annoFieldToField xs
    AnnoType t  -> t
    where
        annoFieldToField :: AnnoADTField -> AdtField
        annoFieldToField field = case field of
            AST.ADTFieldType t           -> FieldType t
            AST.ADTFieldNull             -> FieldNull
            AST.ADTFieldMember symbol ts -> FieldCtor ts


buildTypeImportMap :: BoM (Map.Map Symbol Type) m => [IRGenState] -> m ()
buildTypeImportMap imports = do
    forM_ imports $ \imprt -> do
        forM_ (Map.elems $ irTypeMap imprt) $ \symbol -> do
            False <- Map.member symbol <$> get
            modify $ Map.insert symbol $ irTypeDefs imprt Map.! symbol


buildFuncImportMap :: BoM (Map.Map Symbol FuncKey) m => [IRGenState] -> m ()
buildFuncImportMap imports = do
    forM_ imports $ \imprt -> do
        forM_ (Map.toList $ irFuncMap imprt) $ \(key, symbol) -> do
            False <- Map.member symbol <$> get
            modify $ Map.insert symbol key


buildCtorImportMap :: BoM (Map.Map Symbol (Type, Int)) m => [IRGenState] -> m ()
buildCtorImportMap imports = do
    forM_ imports $ \imprt -> do
        forM_ (Map.toList $ irCtorDefs imprt) $ \(symbol, (t, i)) -> do
            when (Symbol.mod symbol == irModuleName imprt) $ do
                modify $ Map.insert symbol (t, i)


buildCtorMap :: BoM (Map.Map Symbol (Type, Int)) m => [(Symbol, AnnoType)] -> m ()
buildCtorMap list = do
    forM_ list $ \(symbol, anno) -> case anno of
        AnnoEnum xs -> forM_ (zip xs [0..]) $ \(x, i) ->
            modify $ Map.insert x (Type.Typedef symbol, i)
        AnnoADT xs -> forM_ (zip xs [0..]) $ \(x, i) -> case x of
            ADTFieldMember s t -> modify $ Map.insert s (Type.Typedef symbol, i)
            _ -> return ()
        AnnoTuple xs -> forM_ (zip xs [0..]) $ \((s, t), i) -> 
            modify $ Map.insert s (Type.Typedef symbol,  i)
        _ -> return ()


resolveAsts :: BoM s m => [AST] -> [IRGenState] -> m (ResolvedAst, ResolveState)
resolveAsts asts imports = withErrorPrefix "resolve: " $ do
    runBoMTExcept (initResolveState imports (astModuleName $ head asts) Map.empty) f
    where
        f :: BoM ResolveState m => m ResolvedAst
        f = do
            let moduleName = astModuleName $ head asts
            assert (all (== moduleName) $ map astModuleName asts) "module name mismatch"
            let typedefs = [ stmt | stmt@(AST.Typedef _ _ _) <- concat $ map astStmts asts ]
            let funcdefs = [ stmt | stmt@(AST.FuncDef _ _ _ _ _ _) <- concat $ map astStmts asts ]
            assert ((length typedefs + length funcdefs) == length (concat $ map astStmts asts)) "only typedefs and funcdefs allowed"

            forM_ funcdefs $ \(FuncDef pos mparam symbol params retty blk) -> do
                resm <- lookm (Sym $ sym symbol) KeyFunc
                when (isNothing resm) $ define (sym symbol) KeyFunc (Sym $ sym $ symbol)

            (_, typeImportMap) <- runBoMTExcept Map.empty (buildTypeImportMap imports)
            (_, funcImportMap) <- runBoMTExcept Map.empty (buildFuncImportMap imports)
            (_, ctorImportMap) <- runBoMTExcept Map.empty (buildCtorImportMap imports)

            typeDefsMap <- Map.fromList <$> mapM resolveTypeDef typedefs
            funcDefsMap <- Map.fromList <$> mapM resolveFuncDef funcdefs
            (_, ctorMap) <- runBoMTExcept Map.empty (buildCtorMap $ Map.toList typeDefsMap)

            return $ ResolvedAst
                { moduleName  = moduleName
                , typeImports = typeImportMap
                , ctorImports = ctorImportMap
                , funcImports = funcImportMap
                , funcDefs    = funcDefsMap
                , typeDefs    = Map.map annoToType typeDefsMap
                , ctorDefs    = ctorMap
                }



resolveFuncDef :: BoM ResolveState m => AST.Stmt -> m (Symbol, FuncBody)
resolveFuncDef (FuncDef pos params (Sym sym) args retty blk) = do
    -- use a new symbol table for variables in every function
    oldSymTabVar <- gets symTabVar
    modify $ \s -> s { symTabVar = SymTab.initSymTab }

    pushSymTab
    params' <- mapM resolve params
    args' <- mapM resolve args
    retty' <- resolve retty
    blk' <- resolve blk
    popSymTab

    modify $ \s -> s { symTabVar = oldSymTabVar }
    symbol' <- genSymbol sym

    let funcBody = FuncBody {
        funcParams = params' ,
        funcArgs   = args' ,
        funcRetty  = retty' , 
        funcStmts  = [blk']
        }
    return (symbol', funcBody)


resolveTypeDef :: BoM ResolveState m => AST.Stmt -> m (Symbol, AST.AnnoType)
resolveTypeDef (AST.Typedef pos (Sym sym) anno) = do
    symbol <- genSymbol sym
    define sym KeyType symbol
    define sym KeyFunc symbol
    anno' <- case anno of
        AnnoType t -> AnnoType <$> resolve t

        AnnoTuple xs -> do 
            xs' <- forM xs $ \(Sym s, t) -> do
                s' <- genSymbol s
                t' <- resolve t
                return (s', t')
            return $ AnnoTuple xs'

        AnnoADT xs -> do
            xs' <- forM xs $ \x -> case x of
                ADTFieldMember (Sym s) ts -> do
                    s' <- genSymbol s
                    define s KeyFunc s'
                    ts' <- mapM resolve ts
                    return $ ADTFieldMember s' ts'
                ADTFieldType t -> ADTFieldType <$> resolve t
                ADTFieldNull -> return ADTFieldNull
            return $ AnnoADT xs'

        AnnoEnum ss -> do
            ss' <- forM ss $ \(Sym s) -> do
                s' <- genSymbol s
                define s KeyFunc s'
                return s'
            return $ AnnoEnum ss'

        _ -> fail $ "invalid anno: " ++ show anno

    return (symbol, anno')




instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        ExprStmt (Call pos params (Sym "print") exprs) -> do
            exprs' <- mapM resolve exprs
            params' <- mapM resolve params
            case (params', exprs') of
                ([], es) -> return $ Print pos es
                _        -> fail "incorrect print call"

        ExprStmt callExpr -> ExprStmt <$> resolve callExpr

        Block stmts -> do
            pushSymTab
            stmts' <- mapM resolve stmts
            popSymTab
            return $ Block stmts'

        Return pos mexpr -> case mexpr of
            Nothing -> return stmt
            Just expr -> Return pos . Just <$> resolve expr

        Assign pos pat expr -> do
            expr' <- resolve expr 
            pat' <- resolve pat
            return $ Assign pos pat' expr'
        
        If pos condition stmt melse -> do
            pushSymTab
            condition' <- resolve condition
            stmt' <- resolve stmt
            melse' <- maybe (return Nothing) (fmap Just . resolve) melse
            popSymTab
            return $ If pos condition' stmt' melse'

        While pos condition stmt -> do
            pushSymTab
            condition' <- resolve condition
            stmt' <- resolve stmt
            popSymTab
            return $ While pos condition' stmt' 

        Set pos index expr -> do
            index' <- resolve index
            expr' <- resolve expr
            return $ Set pos index' expr'

        Switch pos expr cases -> do
            expr' <- resolve expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pushSymTab
                pat' <- resolve pat
                stmt' <- resolve stmt
                popSymTab
                return (pat', stmt')
            return $ Switch pos expr' cases'
        
        For pos expr mpattern blk -> do
            pushSymTab
            expr' <- resolve expr
            mpattern' <- maybe (return Nothing) (fmap Just . resolve) mpattern
            blk' <- resolve blk
            popSymTab
            return $ For pos expr' mpattern' blk'

        Data pos (Sym sym) typ mexpr -> do
            symbol <- genSymbol sym
            defineVar sym symbol
            typ' <- resolve typ
            mexpr' <- maybe (return Nothing) (fmap Just . resolve) mexpr
            return $ Data pos symbol typ' mexpr'

--        _ -> return stmt
        _ -> fail $ show stmt


instance Resolve Pattern where
    resolve pattern = withPos pattern $ case pattern of
        PatIgnore pos -> return $ PatIgnore pos
        PatIdent pos (Sym sym) -> do
            symbol <- genSymbol sym
            defineVar sym symbol
            return $ PatIdent pos symbol

        PatField pos symbol pats -> do
            pats' <- mapM resolve pats
            symbol' <- look symbol KeyFunc -- TODO bit of a hack
            return $ PatField pos symbol' pats' -- TODO

        PatTypeField pos typ pat -> do
            pat' <- resolve pat
            typ' <- resolve typ
            return $ PatTypeField pos typ' pat'

        PatTuple pos pats -> PatTuple pos <$> mapM resolve pats

        PatLiteral expr -> PatLiteral <$> resolve expr

        PatGuarded pos pat expr -> do
            pat' <- resolve pat
            expr' <- resolve expr
            return $ PatGuarded pos pat' expr'

        PatArray pos pats -> PatArray pos <$> mapM resolve pats

        PatAnnotated pat typ -> do
            pat' <- resolve pat
            typ' <- resolve typ
            return $ PatAnnotated pat' typ'

        PatNull pos -> return $ PatNull pos

        _ -> error $ "invalid pattern: " ++ show pattern



instance Resolve Param where
    resolve (Param pos (Sym sym) typ) = withPos pos $ do
        typ' <- resolve typ
        symbol <- genSymbol sym
        defineVar sym symbol
        return $ Param pos symbol typ'


instance Resolve AdtField where
    resolve adtField = case adtField of
        FieldNull -> return FieldNull
        FieldType t -> FieldType <$> resolve t
        FieldCtor ts -> FieldCtor <$> mapM resolve ts

instance Resolve Type where 
    resolve typ = case typ of
        Void                -> return typ
        _ | isSimple typ    -> return typ
        Type.Table ts       -> Type.Table <$> mapM resolve ts
        Type.Tuple ts       -> Type.Tuple <$> mapM resolve ts
        Type.Array n t      -> Type.Array n <$> resolve t
        Type.Typedef symbol -> Type.Typedef <$> look symbol KeyType
        Type.ADT fs         -> Type.ADT <$>  mapM resolve fs
        Type.Sparse ts      -> Type.Sparse <$> mapM resolve ts
        Type.Range t        -> Type.Range <$> resolve t
        _ -> fail $ "resolve type: " ++ show typ

instance Resolve Expr where
    resolve expr = withPos expr $ case expr of
        Ident pos symbol      -> Ident pos <$> lookVar symbol
        Prefix pos op expr -> Prefix pos op <$> resolve expr
        AST.Char pos c -> return expr
        AST.Int pos n -> return expr
        AST.Bool pos b -> return expr
        Float pos f -> return expr
        AST.Tuple pos exprs -> AST.Tuple pos <$> mapM resolve exprs
        AST.Initialiser pos exprs -> AST.Initialiser pos <$> mapM resolve exprs
        AST.String pos s -> return expr


        Call pos params symbol exprs -> do
            exprs' <- mapM resolve exprs
            params' <- mapM resolve params
            case symbol of
                Sym s | s `elem` ["push", "pop", "len", "clear", "delete", "unsafe_ptr", "unsafe_ptr_from_int"] -> do 
                    return $ Builtin pos params' s exprs'
                _ -> do
                    symbol' <- look symbol KeyFunc
                    return $ Call pos params' symbol' exprs'

        Infix pos op exprA exprB -> do
            exprA' <- resolve exprA
            exprB' <- resolve exprB
            return $ Infix pos op exprA' exprB'

        Subscript pos e1 e2 -> do
            e1' <- resolve e1
            e2' <- resolve e2
            return $ Subscript pos e1' e2'

        Conv pos typ exprs -> do
            typ' <- resolve typ
            exprs' <- mapM resolve exprs
            return $ Conv pos typ' exprs'

        Field pos expr sym -> do
            expr' <- resolve expr
            return $ Field pos expr' sym

        AExpr typ expr -> do
            typ' <- resolve typ
            expr' <- resolve expr
            return $ AExpr typ' expr'

        Null pos -> return (Null pos)

        AST.ADT pos expr -> AST.ADT pos <$> resolve expr

        Match pos expr pat -> do
            expr' <- resolve expr
            pat' <- resolve pat
            return $ Match pos expr' pat'

        AST.Range pos mexpr mexpr1 mexpr2 -> do
            mexpr' <- maybe (return Nothing) (fmap Just . resolve) mexpr
            mexpr1' <- maybe (return Nothing) (fmap Just . resolve) mexpr1
            mexpr2' <- maybe (return Nothing) (fmap Just . resolve) mexpr2
            return $ AST.Range pos mexpr' mexpr1' mexpr2'

        _ -> fail $ "invalid expression: " ++ show expr
