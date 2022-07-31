{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Apply where

import Type
import qualified AST as S
import Collect
import qualified Data.Map as Map
import qualified SymTab

-- constraint:   (t1, t2) or (x, y)
-- substitution: (id, t) or (x, u)
substitute :: Type -> Int -> Type -> Type
substitute u x typ = case typ of
    Type i | i == x -> u
    Type _          -> typ
    Bool            -> typ
    I64             -> typ
    I32             -> typ
    F32             -> typ
    F64             -> typ
    Char            -> typ
    Table ts        -> Table $ map (substitute u x) ts
    Tuple ts        -> Tuple $ map (substitute u x) ts
    Array n t       -> Array n (substitute u x t)
    Void            -> typ
    Typedef symbol  -> typ
    ADT tss         -> ADT $ map (map (substitute u x)) tss
    UnsafePtr t     -> UnsafePtr (substitute u x t)
    _               -> error (show typ)


-- Apply represents taking a list of substitutions and applying them to all types in an object.
class Apply a where
    apply :: [(Int, Type)] -> a -> a


instance Apply Collect.SymTab where
    apply subs symTab =
        SymTab.mapKeys (apply subs) $ SymTab.map (apply subs) symTab

instance Apply Collect.Object where
    apply subs object = case object of
        ObjVar t    -> ObjVar (apply subs t)
        ObjType t   -> ObjType (apply subs t)
        ObjFunc     -> ObjFunc
        ObjMember i -> ObjMember i

instance Apply Collect.SymKey where
    apply subs key = case key of
        KeyVar        -> KeyVar
        KeyType       -> KeyType
        KeyFunc ts rt -> KeyFunc (map (apply subs) ts) (apply subs rt)
        KeyMember t   -> KeyMember (apply subs t)

instance Apply Constraint where
    apply subs (Constraint p t1 t2) = Constraint p (apply subs t1) (apply subs t2)

instance Apply Type where
    apply subs t = foldr (\(x, u) z -> substitute u x z) t subs
    
instance Apply S.Param where
    apply subs (S.Param p n t) = S.Param p n (apply subs t)

instance Apply S.Expr where
    apply subs expr = case expr of
        S.AExpr t e                -> S.AExpr (apply subs t) (apply subs e)
        S.Infix pos op expr1 expr2 -> S.Infix pos op (apply subs expr1) (apply subs expr2)
        S.Tuple pos exprs          -> S.Tuple pos $ map (apply subs) exprs
        S.Ident pos sym            -> expr
        S.Char  pos c              -> expr
        S.Int   pos n              -> expr
        S.Prefix pos op expr1      -> S.Prefix pos op (apply subs expr1)
        S.Call  pos sym exprs      -> S.Call pos sym $ map (apply subs) exprs
        S.Conv  pos t exprs        -> S.Conv pos (apply subs t) $ map (apply subs) exprs
        S.Copy  pos e              -> S.Copy pos (apply subs e)
        S.Len   pos e              -> S.Len  pos (apply subs e)
        S.Zero  pos                -> expr
        S.Bool  pos b              -> expr
        S.Subscript pos e1 e2      -> S.Subscript pos (apply subs e1) (apply subs e2)
        S.String pos s             -> expr
        S.Member pos e s           -> S.Member pos (apply subs e) s
        S.Float pos f              -> expr
        S.Table pos ess            -> S.Table pos $ map (map (apply subs)) ess
        S.TupleIndex pos e i       -> S.TupleIndex pos (apply subs e) i
        S.Range pos e me1 me2      -> S.Range pos (apply subs e) (fmap (apply subs) me1) (fmap (apply subs) me2)
        S.UnsafePtr p e            -> S.UnsafePtr p (apply subs e)
        _                          -> error $ show expr

instance Apply S.Condition where
    apply subs cnd = case cnd of
        S.CondExpr expr      -> S.CondExpr (apply subs expr)
        S.CondMatch pat expr -> S.CondMatch (apply subs pat) (apply subs expr)
        _               -> error $ show cnd

instance Apply S.Pattern where
    apply subs pattern = case pattern of
        S.PatIdent p s       -> pattern
        S.PatLiteral e       -> S.PatLiteral (apply subs e)
        S.PatGuarded p pat e -> S.PatGuarded p (apply subs pat) (apply subs e)
        S.PatField p s pats  -> S.PatField p s $ map (apply subs) pats
        S.PatTuple p pats    -> S.PatTuple p $ map (apply subs) pats
        S.PatIgnore p        -> S.PatIgnore p
        S.PatArray p pats    -> S.PatArray p $ map (apply subs) pats
        S.PatAnnotated pat typ -> S.PatAnnotated (apply subs pat) (apply subs typ)
        _                    -> error $ show pattern

instance Apply S.Append where
    apply subs app = case app of
        S.AppendTable p ap e -> S.AppendTable p (apply subs ap) (apply subs e)
        S.AppendIndex index  -> S.AppendIndex (apply subs index)

instance Apply S.Index where
    apply subs index = case index of
        S.IndIdent p sym -> index
        S.IndArray p ind e -> S.IndArray p (apply subs ind) (apply subs e)
        _ -> error $ show index

instance Apply S.Stmt where
    apply subs stmt = case stmt of
        S.Block stmts             -> S.Block $ map (apply subs) stmts
        S.Return pos mexpr        -> S.Return pos $ fmap (apply subs) mexpr
        S.Assign pos pat expr     -> S.Assign pos (apply subs pat) (apply subs expr)
        S.AppendStmt app          -> S.AppendStmt (apply subs app)
        S.Set pos index e         -> S.Set pos (apply subs index) (apply subs e)
        S.While pos cnd blk       -> S.While pos (apply subs cnd) (apply subs blk)
        S.CallStmt pos sym es     -> S.CallStmt pos sym $ map (apply subs) es
        S.Print pos es            -> S.Print pos $ map (apply subs) es

        S.FuncDef pos sym params retty block ->
            S.FuncDef pos sym (map (apply subs) params) (apply subs retty) (apply subs block)

        S.If pos cnd block melse ->
            S.If pos (apply subs cnd) (apply subs block) $ fmap (apply subs) melse

        S.Typedef _ _ _ -> stmt -- leave this for now
        
        S.Switch pos expr cases ->
            S.Switch pos (apply subs expr) [(apply subs p, apply subs s) | (p, s) <- cases]

        S.For pos symbol (Just t) expr mpat blk ->
            S.For pos symbol (Just $ apply subs t) (apply subs expr) (fmap (apply subs) mpat) (apply subs blk)
            

instance Apply S.AST where
    apply subs ast = ast { S.astStmts = map (apply subs) (S.astStmts ast) }
