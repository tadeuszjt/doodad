{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Apply where

import Type as T
import AST as S
import Collect
import qualified Data.Map as Map

-- constraint:   (t1, t2) or (x, y)
-- substitution: (id, t) or (x, u)
substitute :: Type -> Int -> Type -> Type
substitute u x t = case t of
    Type i | i == x  -> u
    Type _           -> t
    T.Bool           -> t
    I64              -> t
    I32              -> t
    F32              -> t
    F64              -> t
    T.Char           -> t
    T.Table [T.Char] -> t
    T.Tuple ts       -> T.Tuple $ map (substitute u x) ts
    Void             -> t
    T.Typedef symbol -> t
    _                -> error (show t)


-- Apply represents taking a list of substitutions and applying them to all types in an object.
class Apply a where
    apply :: [(Int, Type)] -> a -> a


instance Apply Collect.SymTab where
    apply subs symTab = map (Map.map (Map.map (apply subs))) symTab

instance Apply Collect.Object where
    apply subs object = case object of
        ObjVar t -> ObjVar (apply subs t)
        ObjType t -> ObjType (apply subs t)
        ObjFunc t -> ObjFunc (apply subs t)
        ObjMember i -> ObjMember i

instance Apply Constraint where
    apply subs (Constraint p t1 t2) = Constraint p (apply subs t1) (apply subs t2)

instance Apply Type where
    apply subs t = foldr (\(x, u) z -> substitute u x z) t subs
    
instance Apply Param where
    apply subs (Param p n t) = Param p n (apply subs t)

instance Apply Expr where
    apply subs expr = case expr of
        AExpr t e                -> AExpr (apply subs t) (apply subs e)
        Infix pos op expr1 expr2 -> Infix pos op (apply subs expr1) (apply subs expr2)
        S.Tuple pos exprs        -> S.Tuple pos $ map (apply subs) exprs
        Ident pos sym            -> expr
        S.Char  pos c            -> expr
        Int   pos n              -> expr
        Prefix pos op expr1      -> Prefix pos op (apply subs expr1)
        Call  pos sym exprs      -> Call pos sym $ map (apply subs) exprs
        Conv  pos t exprs        -> Conv pos (apply subs t) $ map (apply subs) exprs
        Copy  pos e              -> Copy pos (apply subs e)
        Len   pos e              -> Len  pos (apply subs e)
        S.Bool  pos b            -> expr
        Subscript pos e1 e2      -> Subscript pos (apply subs e1) (apply subs e2)
        String pos s             -> expr
        Member pos e s           -> Member pos (apply subs e) s
        S.Float pos f            -> expr
        _                          -> error $ show expr

instance Apply Condition where
    apply subs cnd = case cnd of
        CondExpr expr -> CondExpr (apply subs expr)
        _               -> error $ show cnd

instance Apply Pattern where
    apply subs pattern = case pattern of
        PatIdent p s       -> pattern
        PatLiteral e       -> PatLiteral (apply subs e)
        PatGuarded p pat e -> PatGuarded p (apply subs pat) (apply subs e)
        _                    -> error $ show pattern

instance Apply Append where
    apply subs app = case app of
        AppendTable p ap e -> AppendTable p (apply subs ap) (apply subs e)
        AppendIndex index   -> AppendIndex (apply subs index)

instance Apply Index where
    apply subs index = case index of
        IndIdent p sym -> index
        IndArray p ind e -> IndArray p (apply subs ind) (apply subs e)
        _ -> error $ show index

instance Apply Stmt where
    apply subs stmt = case stmt of
        Block stmts             -> Block $ map (apply subs) stmts
        Return pos mexpr        -> Return pos $ fmap (apply subs) mexpr
        Assign pos pat expr     -> Assign pos (apply subs pat) (apply subs expr)
        AppendStmt app          -> AppendStmt (apply subs app)
        Set pos index e         -> Set pos (apply subs index) (apply subs e)
        While pos cnd blk       -> While pos (apply subs cnd) (apply subs blk)
        CallStmt pos sym es     -> CallStmt pos sym $ map (apply subs) es
        Print pos es            -> Print pos $ map (apply subs) es

        FuncDef pos sym params retty block ->
            FuncDef pos sym (map (apply subs) params) (apply subs retty) (apply subs block)

        Extern pos name sym params retty ->
            Extern pos name sym (map (apply subs) params) (apply subs retty)

        If pos cnd block melse ->
            If pos (apply subs cnd) (apply subs block) $ fmap (apply subs) melse

        S.Typedef _ _ _ -> stmt -- leave this for now

instance Apply AST where
    apply subs ast = ast { astStmts = map (apply subs) (astStmts ast) }
