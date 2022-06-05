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
    | KeyFunc [Type]
    | KeyMember Symbol
    deriving (Show, Eq, Ord)


type SymTab = SymTab.SymTab String SymKey Symbol

data ResolveState
    = ResolveState
        { symTab    :: SymTab
        , curRetty  :: Type
        , imports   :: Map.Map FilePath SymTab
        , modName   :: String
        }

initResolveState imp modName = ResolveState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , imports    = imp
    , modName    = modName
    }


look :: BoM ResolveState m => Symbol -> SymKey -> m Symbol
look symbol key = case symbol of
    Sym sym -> do
        lm <- gets $ SymTab.lookup sym key . symTab
        modName <- gets modName
        case lm of
            Just res -> return res
            Nothing -> do
                ls <- gets $ catMaybes . map (SymTab.lookup sym key) . Map.elems . imports
                case ls of
                    [res] -> return res
                    []  -> fail $ show symbol ++ " not defined"
                    _   -> fail $ show symbol ++ " is ambiguous"
        

define :: BoM ResolveState m => String -> SymKey -> m Symbol
define sym key = do
    resm <- gets $ SymTab.lookupHead sym key . symTab
    when (isJust resm) $ fail $ sym ++ " already defined"
    level <- gets (length . symTab)
    modName <- gets modName
    let symbol = SymResolved modName sym level
    modify $ \s -> s { symTab = SymTab.insert sym key symbol (symTab s) }
    return symbol


pushSymTab :: BoM ResolveState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM ResolveState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


instance Resolve AST where
    resolve ast = do
        let (typedefs, stmts'') = partition isTypedef (astStmts ast)
        let (funcdefs, stmts') = partition isFuncdef stmts''
        let (externdefs, stmts) = partition isExterndef stmts'

        --forM typedefs $ resolveTypedef

        forM funcdefs $ \(FuncDef pos sym params retty _) ->
            define sym (KeyFunc $ map paramType params)
--
--        forM externdefs $ \(Extern pos name sym params retty) ->
--            define sym (KeyFunc $ map paramType params) (ObjFunc retty)

        astStmts <- mapM resolve (astStmts ast)
        return $ ast { astStmts = astStmts }
        where
            isTypedef :: Stmt -> Bool
            isTypedef (AST.Typedef _ _ _) = True
            isTypedef _                 = False

            isFuncdef :: Stmt -> Bool
            isFuncdef (FuncDef _ _ _ _ _) = True
            isFuncdef _                     = False

            isExterndef :: Stmt -> Bool
            isExterndef (Extern _ _ _ _ _)  = True
            isExterndef _                     = False

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
            symbol <- define sym KeyType
            anno' <- case anno of
                AnnoTuple xs -> do 
                    xs' <- forM xs $ \(s, t) -> do
                        define s (KeyMember symbol)
                        t' <- resolve t
                        return (s, t')
                    return $ AnnoTuple xs'

            return $ AST.Typedef pos symbol anno'



        _ -> return stmt
        _ -> fail $ show stmt


instance Resolve Pattern where
    resolve pattern = withPos pattern $ case pattern of
        PatIdent pos sym -> do
            define sym KeyVar
            return $ PatIdent pos sym



instance Resolve Param where
    resolve (Param pos sym typ) = withPos pos $ do
        typ' <- resolve typ
        define sym KeyVar
        return $ Param pos sym typ'

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
        Call pos symbol exprs -> Call pos symbol <$> mapM resolve exprs

        _ -> return expr

        _ -> fail (show expr)
