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


class Resolve a where
    resolve :: BoM ResolveState m => a -> m a

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc
    deriving (Show, Eq, Ord)


type SymTab = SymTab.SymTab String SymKey Symbol

data ResolveState
    = ResolveState
        { symTab    :: SymTab
        , curRetty  :: Type
        , imports   :: Map.Map FilePath SymTab
        , modName   :: String
        , supply    :: Map.Map (String, String) Int
        }

initResolveState imp modName = ResolveState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , imports    = imp
    , modName    = modName
    , supply     = Map.empty
    }


look :: BoM ResolveState m => Symbol -> SymKey -> m Symbol
look symbol key = do
    lm <- lookm symbol key
    assert (isJust lm) $ show symbol ++ " isn't defined"
    return $ fromJust lm


lookm :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Symbol)
lookm symbol key = case symbol of
    Sym sym -> do
        imports <- gets $ Map.elems . imports
        symTab <- gets symTab
        return $ lookupSymKey sym key symTab imports

    SymQualified mod sym -> do
        modName <- gets modName
        symTab <- gets symTab
        imports <- gets $ Map.elems . imports
        if mod == modName then  return $ lookupSymKey sym key symTab []
        else if mod == "c" then return (Just symbol)
        else                    return $ lookupSymKey sym key symTab imports

    _ -> fail $ show (symbol, key)


genSymbol :: BoM ResolveState m => String -> m Symbol
genSymbol sym = do  
    modName <- gets modName
    im <- gets $ Map.lookup (modName, sym) . supply
    i <- case im of
        Nothing -> do
            modify $ \s -> s { supply = Map.insert (modName, sym) 1 (supply s) }
            return 0
        Just i -> do
            modify $ \s -> s { supply = Map.insert (modName, sym) (i + 1) (supply s) }
            return i
    let symbol = SymResolved modName sym i
    return symbol
        

define :: BoM ResolveState m => String -> SymKey -> Symbol -> m ()
define sym key symbol = do
    resm <- gets $ SymTab.lookupHead sym key . symTab
    when (isJust resm) $ fail $ sym ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key symbol (symTab s) }


pushSymTab :: BoM ResolveState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM ResolveState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


instance Resolve AST where
    resolve ast = do
        let (typedefs, stmts'') = partition isTypedef (astStmts ast)
        let (funcdefs, stmts) = partition isFuncdef stmts''

        forM funcdefs $ \(FuncDef pos mparam sym params retty blk) -> do
            resm <- lookm (Sym sym) KeyFunc
            when (isNothing resm) $ define sym KeyFunc (Sym sym)

        astStmts <- mapM resolve $ typedefs ++ stmts''
        return $ ast { astStmts = astStmts }
        where
            isTypedef :: Stmt -> Bool
            isTypedef (AST.Typedef _ _ _) = True
            isTypedef _                   = False

            isFuncdef :: Stmt -> Bool
            isFuncdef (FuncDef _ _ _ _ _ _) = True
            isFuncdef _                     = False


instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        FuncDef pos mparam sym params retty blk -> do
            pushSymTab
            mparam' <- maybe (return Nothing) (fmap Just . resolve) mparam
            params' <- mapM resolve params
            retty' <- resolve retty
            blk' <- resolve blk
            popSymTab

            return $ FuncDef pos mparam' sym params' retty' blk'


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
        
        AST.Typedef pos (Sym sym) anno -> do
            symbol <- genSymbol sym
            define sym KeyType symbol
            define sym KeyFunc symbol
            anno' <- case anno of
                AnnoType t -> AnnoType <$> resolve t

                AnnoTuple xs -> do 
                    xs' <- forM xs $ \(s, t) -> do
                        t' <- resolve t
                        return (s, t')
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

                _ -> fail $ "invalid anno: " ++ show anno

            return $ AST.Typedef pos symbol anno'

        AppendStmt append -> AppendStmt <$> resolve append
        
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

        Print pos exprs -> Print pos <$> mapM resolve exprs

        Switch pos expr cases -> do
            expr' <- resolve expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pushSymTab
                pat' <- resolve pat
                stmt' <- resolve stmt
                popSymTab
                return (pat', stmt')
            return $ Switch pos expr' cases'
        
        CallStmt pos symbol exprs -> do
            exprs' <- mapM resolve exprs
            symbolm <- lookm symbol KeyFunc
            case symbolm of
                Just symbol' -> return $ CallStmt pos symbol' exprs'
                Nothing -> do
                    symbolmm <- lookm symbol KeyType
                    case symbolmm of
                        Nothing ->      fail $ show symbol ++ " isn't defined"
                        Just symbol' -> return $ CallStmt pos symbol' exprs'

        For pos (Sym sym) Nothing expr mpattern blk -> do
            pushSymTab
            symbol <- genSymbol sym
            define sym KeyVar symbol
            expr' <- resolve expr
            mpattern' <- maybe (return Nothing) (fmap Just . resolve) mpattern
            blk' <- resolve blk
            popSymTab
            return $ For pos symbol Nothing expr' mpattern' blk'

        Data pos (Sym sym) typ -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
            typ' <- resolve typ
            return $ Data pos symbol typ'

--        _ -> return stmt
        _ -> fail $ show stmt

instance Resolve Condition where
    resolve condition = withPos condition $ case condition of
        CondExpr expr -> CondExpr <$> resolve expr
        CondMatch pat expr -> do
            pat' <- resolve pat
            expr' <- resolve expr
            return $ CondMatch pat' expr'


instance Resolve Index where
    resolve index = withPos index $ case index of
        IndIdent pos symbol -> IndIdent pos <$> look symbol KeyVar
        IndArray pos ind expr -> do
            ind' <- resolve ind
            expr' <- resolve expr
            return $ IndArray pos ind' expr'
        _ -> fail $ "cannot resolve: " ++ show index

instance Resolve Append where
    resolve append = withPos append $ case append of
        AppendIndex index -> AppendIndex <$> resolve index
        AppendTable pos index expr -> do
            index' <- resolve index
            expr' <- resolve expr
            return $ AppendTable pos index' expr'


instance Resolve Pattern where
    resolve pattern = withPos pattern $ case pattern of
        PatIgnore pos -> return $ PatIgnore pos
        PatIdent pos (Sym sym) -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
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
        define sym KeyVar symbol
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
        Type.KeyMap ts      -> Type.KeyMap <$> mapM resolve ts

        _ -> fail $ "resolve type: " ++ show typ

instance Resolve Expr where
    resolve expr = withPos expr $ case expr of
        Ident pos symbol      -> Ident pos <$> look symbol KeyVar
        Prefix pos op expr -> Prefix pos op <$> resolve expr
        AST.Char pos c -> return expr
        Len pos expr -> Len pos <$> resolve expr
        Copy pos expr -> Copy pos <$> resolve expr
        AST.UnsafePtr pos expr -> AST.UnsafePtr pos <$> resolve expr
        AST.Int pos n -> return expr
        AST.Bool pos b -> return expr
        Float pos f -> return expr
        AST.Tuple pos exprs -> AST.Tuple pos <$> mapM resolve exprs
        AST.Table pos exprss -> AST.Table pos <$> mapM (mapM resolve) exprss
        AST.String pos s -> return expr

        Call pos symbol exprs -> do
            exprs' <- mapM resolve exprs
            symbolm <- lookm symbol KeyFunc
            case symbolm of
                Just symbol' -> return $ Call pos symbol' exprs'
                Nothing -> do
                    symbolmm <- lookm symbol KeyType
                    case symbolmm of
                        Nothing ->      fail $ show symbol ++ " isn't defined"
                        Just symbol' -> return $ Call pos symbol' exprs'

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

        Member pos expr sym -> do
            expr' <- resolve expr
            return $ Member pos expr' sym

        AExpr typ expr -> do
            typ' <- resolve typ
            expr' <- resolve expr
            return $ AExpr typ' expr'

        TupleIndex pos expr i -> do
            expr' <- resolve expr
            return $ TupleIndex pos expr' i

        Range pos expr mexpr1 mexpr2 -> do
            expr' <- resolve expr
            mexpr1' <- maybe (return Nothing) (fmap Just . resolve) mexpr1
            mexpr2' <- maybe (return Nothing) (fmap Just . resolve) mexpr2
            return $ Range pos expr' mexpr1' mexpr2'

        Zero pos -> return (Zero pos)

        Null pos -> return (Null pos)

        AST.ADT pos expr -> AST.ADT pos <$> resolve expr

        CallMember pos expr ident exprs -> do
            expr' <- resolve expr
            exprs' <- mapM resolve exprs
            return $ CallMember pos expr' ident exprs'


        --_ -> return expr

        _ -> fail $ "invalid expression: " ++ show expr
