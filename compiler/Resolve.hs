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
    case lm of
        Just symbol -> return symbol
        Nothing     -> fail $ show symbol ++ " " ++ show key ++ " isn't defined"

lookm :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Symbol)
lookm symbol key = case symbol of
    Sym sym -> do
        lm <- gets $ SymTab.lookup sym key . symTab
        case lm of
            Just res -> return (Just res)
            Nothing -> do
                ls <- gets $ catMaybes . map (SymTab.lookup sym key) . Map.elems . imports
                case ls of
                    [res] -> return (Just res)
                    [] -> return Nothing
                    _   -> fail $ show symbol ++ " is ambiguous"

    SymQualified mod sym -> do
        modName <- gets modName
        if mod == modName then do
            lm <- gets $ SymTab.lookup sym key . symTab
            case lm of
                Just res -> return (Just res)
                Nothing  -> return Nothing
        else if mod == "c" then do
            return (Just symbol)
        else do
            ls <- gets $ catMaybes . map (SymTab.lookup sym key) . Map.elems . imports
            case ls of
                [res] -> return (Just res)
                []  -> return Nothing
                _   -> fail $ show symbol ++ " is ambiguous"

    _ -> fail $ show (symbol, key)
    where
        lookmImports :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Symbol)
        lookmImports (Sym sym) key = do
            ls <- gets $ catMaybes . map (SymTab.lookup sym key) . Map.elems . imports
            case ls of
                [res] -> return (Just res)
                []  -> return Nothing
                _   -> fail $ show symbol ++ " is ambiguous"
            


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

        --forM typedefs $ resolveTypedef

        funcdefs' <- forM funcdefs $ \(FuncDef pos sym params retty blk) -> do
            resm <- lookm (Sym sym) KeyFunc
            case resm of
                Just symbol -> return $ FuncDef pos sym params retty blk
                Nothing     -> do
                    --symbol <- genSymbol sym
                    let symbol = (Sym sym)
                    define sym KeyFunc symbol
                    return $ FuncDef pos sym params retty blk

        astStmts <- mapM resolve $ typedefs ++ funcdefs' ++ stmts
        return $ ast { astStmts = astStmts }
        where
            isTypedef :: Stmt -> Bool
            isTypedef (AST.Typedef _ _ _) = True
            isTypedef _                 = False

            isFuncdef :: Stmt -> Bool
            isFuncdef (FuncDef _ _ _ _ _) = True
            isFuncdef _                     = False

instance Resolve Stmt where
    resolve stmt = withPos stmt $ case stmt of
        FuncDef pos sym params retty blk -> do
            pushSymTab
            params' <- mapM resolve params
            retty' <- resolve retty
            blk' <- resolve blk
            popSymTab
            return $ FuncDef pos sym params' retty' blk'

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
                AnnoTuple xs -> do 
                    xs' <- forM xs $ \(s, t) -> do
                        t' <- resolve t
                        return (s, t')
                    return $ AnnoTuple xs'
                AnnoADT xs -> do
                    xs' <- forM xs $ \(Sym s, t) -> do
                        s' <- genSymbol s
                        define s KeyFunc s'
                        t' <- resolve t
                        return (s', t')
                    return $ AnnoADT xs'
                _ -> fail (show anno)

            return $ AST.Typedef pos symbol anno'

        AppendStmt append -> AppendStmt <$> resolve append
        
        If pos condition stmt melse -> do
            condition' <- resolve condition
            stmt' <- resolve stmt
            melse' <- maybe (return Nothing) (fmap Just . resolve) melse
            return $ If pos condition' stmt' melse'

        While pos condition stmt -> do
            condition' <- resolve condition
            stmt' <- resolve stmt
            return $ While pos condition' stmt' 

        Set pos index expr -> do
            index' <- resolve index
            expr' <- resolve expr
            return $ Set pos index' expr'

        Print pos exprs -> Print pos <$> mapM resolve exprs

        Switch pos expr cases -> do
            expr' <- resolve expr
            pushSymTab
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- resolve pat
                stmt' <- resolve stmt
                return (pat', stmt')
            popSymTab
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

        For pos (Sym sym) Nothing expr blk -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
            expr' <- resolve expr
            blk' <- resolve blk
            return $ For pos symbol Nothing expr' blk'

--        _ -> return stmt
        _ -> fail $ show stmt

instance Resolve Condition where
    resolve condition = case condition of
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
        PatIdent pos (Sym sym) -> do
            symbol <- genSymbol sym
            define sym KeyVar symbol
            return $ PatIdent pos symbol

        PatField pos symbol pat -> do
            pat' <- resolve pat
            symbol' <- look symbol KeyFunc -- TODO bit of a hack
            return $ PatField pos symbol' pat' -- TODO

        PatTuple pos pats -> PatTuple pos <$> mapM resolve pats

        _ -> fail (show pattern)



instance Resolve Param where
    resolve (Param pos (Sym sym) typ) = withPos pos $ do
        typ' <- resolve typ
        symbol <- genSymbol sym
        define sym KeyVar symbol
        return $ Param pos symbol typ'

instance Resolve Type where 
    resolve typ = case typ of
        Void                -> return typ
        _ | isSimple typ    -> return typ
        Type.Table ts       -> Type.Table <$> mapM resolve ts
        Type.Tuple ts       -> Type.Tuple <$> mapM resolve ts
        Type.Typedef symbol -> Type.Typedef <$> look symbol KeyType

        _ -> fail (show typ)

instance Resolve Expr where
    resolve expr = withPos expr $ case expr of
        Ident pos symbol      -> Ident pos <$> look symbol KeyVar

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

        Prefix pos op expr -> Prefix pos op <$> resolve expr

        AST.Char pos c -> return expr

        Copy pos expr -> Copy pos <$> resolve expr

        AST.Int pos n -> return expr

        AST.Bool pos b -> return expr

        Float pos f -> return expr

        AST.Tuple pos exprs -> AST.Tuple pos <$> mapM resolve exprs
        AST.Table pos exprss -> AST.Table pos <$> mapM (mapM resolve) exprss
        String pos s -> return expr

        Len pos expr -> Len pos <$> resolve expr

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


        --_ -> return expr

        _ -> fail (show expr)