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
    Type _           -> typ
    Void             -> typ
    Typedef symbol   -> typ
    _ | isSimple typ -> typ
    Sparse ts        -> Sparse $ map (substitute u x) ts
    Table ts         -> Table $ map (substitute u x) ts
    Tuple ts         -> Tuple $ map (substitute u x) ts
    Array n t        -> Array n (substitute u x t)
    ADT fs           -> ADT $ map subAdtField fs
    UnsafePtr t      -> UnsafePtr (substitute u x t)
    _                -> error (show typ)
    where
        subAdtField :: AdtField -> AdtField
        subAdtField field = case field of
            FieldNull -> FieldNull
            FieldType t -> FieldType (substitute u x t)
            FieldCtor ts -> FieldCtor $ map (substitute u x) ts


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
        ObjField i -> ObjField i

instance Apply Collect.SymKey where
    apply subs key = case key of
        KeyVar        -> KeyVar
        KeyType       -> KeyType
        KeyFunc ts rt -> KeyFunc (map (apply subs) ts) (apply subs rt)
        KeyField t   -> KeyField (apply subs t)
        KeyMember t ts rt -> KeyMember (apply subs t) (map (apply subs) ts) (apply subs rt)

instance Apply Constraint where
    apply subs (ConsEq t1 t2)       = ConsEq (apply subs t1) (apply subs t2)
    apply subs (ConsBase   t1 t2)   = ConsBase   (apply subs t1) (apply subs t2)
    apply subs (ConsElem   t1 t2)   = ConsElem   (apply subs t1) (apply subs t2)
    apply subs (ConsField t1 i t2) = ConsField (apply subs t1) i (apply subs t2)
    apply subs (ConsAdtMem t1 i j t2) = ConsAdtMem (apply subs t1) i j (apply subs t2)

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
        S.Null  pos                -> expr
        S.Prefix pos op expr1      -> S.Prefix pos op (apply subs expr1)
        S.Call  pos sym exprs      -> S.Call pos sym $ map (apply subs) exprs
        S.Conv  pos t exprs        -> S.Conv pos (apply subs t) $ map (apply subs) exprs
        S.Len   pos e              -> S.Len  pos (apply subs e)
        S.Bool  pos b              -> expr
        S.Subscript pos e1 e2      -> S.Subscript pos (apply subs e1) (apply subs e2)
        S.String pos s             -> expr
        S.Field pos e s           -> S.Field pos (apply subs e) s
        S.Float pos f              -> expr
        S.Table pos ess            -> S.Table pos $ map (map (apply subs)) ess
        S.TupleIndex pos e i       -> S.TupleIndex pos (apply subs e) i
        S.UnsafePtr p e            -> S.UnsafePtr p (apply subs e)
        S.ADT p e                  -> S.ADT p (apply subs e)
        S.CallMember p e ident es  -> S.CallMember p (apply subs e) ident (map (apply subs) es)
        S.Push p e es              -> S.Push p (apply subs e) (map (apply subs) es)
        S.Pop p e es               -> S.Pop p (apply subs e) (map (apply subs) es)
        S.Clear p e                -> S.Clear p (apply subs e)
        S.Delete p e1 e2           -> S.Delete p (apply subs e1) (apply subs e2)
        S.Match p e pat            -> S.Match p (apply subs e) (apply subs pat)
        S.Range p me me1 me2       -> S.Range p (fmap (apply subs) me) (fmap (apply subs) me1) (fmap (apply subs) me2)
        _                          -> error $ show expr


instance Apply S.Pattern where
    apply subs pattern = case pattern of
        S.PatIdent p s       -> pattern
        S.PatLiteral e       -> S.PatLiteral (apply subs e)
        S.PatGuarded p pat e -> S.PatGuarded p (apply subs pat) (apply subs e)
        S.PatField p s pats  -> S.PatField p s $ map (apply subs) pats
        S.PatTypeField p t pat -> S.PatTypeField p (apply subs t) (apply subs pat)
        S.PatTuple p pats    -> S.PatTuple p $ map (apply subs) pats
        S.PatIgnore p        -> S.PatIgnore p
        S.PatArray p pats    -> S.PatArray p $ map (apply subs) pats
        S.PatAnnotated pat typ -> S.PatAnnotated (apply subs pat) (apply subs typ)
        S.PatNull p            -> S.PatNull p
        _                    -> error $ show pattern


instance Apply S.Stmt where
    apply subs stmt = case stmt of
        S.Block stmts           -> S.Block $ map (apply subs) stmts
        S.Return pos mexpr      -> S.Return pos $ fmap (apply subs) mexpr
        S.Assign pos pat expr   -> S.Assign pos (apply subs pat) (apply subs expr)
        S.Set pos index e       -> S.Set pos (apply subs index) (apply subs e)
        S.While pos cnd blk     -> S.While pos (apply subs cnd) (apply subs blk)
        S.ExprStmt e            -> S.ExprStmt (apply subs e)
        S.Print pos es          -> S.Print pos $ map (apply subs) es

        S.FuncDef pos mparam sym params retty block ->
            S.FuncDef pos (fmap (apply subs) mparam) sym (map (apply subs) params) (apply subs retty) (apply subs block)

        S.If pos cnd block melse ->
            S.If pos (apply subs cnd) (apply subs block) $ fmap (apply subs) melse

        S.Typedef _ _ _ -> stmt -- leave this for now
        
        S.Switch pos expr cases ->
            S.Switch pos (apply subs expr) [(apply subs p, apply subs s) | (p, s) <- cases]

        S.For pos expr mpat blk ->
            S.For pos (apply subs expr) (fmap (apply subs) mpat) (apply subs blk)

        S.Data pos symbol typ ->
            S.Data pos symbol (apply subs typ)
            

instance Apply S.AST where
    apply subs ast = ast { S.astStmts = map (apply subs) (S.astStmts ast) }
