{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Apply where

import Type
import qualified AST as S
import Collect
import qualified Data.Map as Map
import qualified SymTab
import qualified Resolve
import States

-- constraint:   (t1, t2) or (x, y)
-- substitution: (id, t) or (x, u)
substitute :: Type -> Int -> Type -> Type
substitute u x typ = case typ of
    Type i | i == x -> u
    Type _           -> typ
    Void             -> typ
    Typedef symbol   -> typ
    _ | isSimple typ -> typ
    Range t          -> Range $ substitute u x t
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

            
instance Apply ResolvedAst where
    apply subs ast = ast { funcDefs = Map.map (apply subs) (funcDefs ast) }


instance Apply FuncBody where
    apply subs body = FuncBody
        { funcParams = map f (funcParams body)
        , funcArgs   = map f (funcArgs body)
        , funcRetty  = f (funcRetty body)
        , funcStmts  = map f (funcStmts body)
        }
        where 
            f :: Apply a => a -> a
            f = apply subs
    

instance Apply Type where
    apply subs t = foldr (\(x, u) z -> substitute u x z) t subs
    
instance Apply Constraint where
    apply subs constraint = case constraint of
        ConsEq t1 t2         -> ConsEq (f t1) (f t2)
        ConsBase t1 t2       -> ConsBase (f t1) (f t2)
        ConsElem t1 t2       -> ConsElem (f t1) (f t2)
        ConsSubscript t1 t2  -> ConsSubscript (f t1) (f t2)
        ConsField t1 i t2    -> ConsField (f t1) i (f t2)
        ConsAdtMem t1 i j t2 -> ConsAdtMem (f t1) i j (f t2)
        where
            f :: Apply a => a -> a
            f = apply subs

instance Apply S.Param where
    apply subs (S.Param p n t) = S.Param p n (apply subs t)

instance Apply S.Expr where
    apply subs expr = case expr of
        S.AExpr t e           -> S.AExpr (f t) (f e)
        S.Infix pos op e1 e2  -> S.Infix pos op (f e1) (f e2)
        S.Tuple pos es        -> S.Tuple pos (map f es)
        S.Ident pos sym       -> S.Ident pos sym
        S.Char  pos c         -> S.Char pos c
        S.Int   pos n         -> S.Int pos n
        S.Null  pos           -> S.Null pos
        S.Prefix pos op e1    -> S.Prefix pos op (f e1)
        S.Conv  pos t es      -> S.Conv pos (f t) (map f es)
        S.Len   pos e         -> S.Len  pos (f e)
        S.Bool  pos b         -> S.Bool pos b
        S.Subscript pos e1 e2 -> S.Subscript pos (f e1) (f e2)
        S.String pos s        -> S.String pos s
        S.Field pos e s       -> S.Field pos (f e) s
        S.Float pos f         -> S.Float pos f
        S.Initialiser pos es  -> S.Initialiser pos (map f es)
        S.TupleIndex pos e i  -> S.TupleIndex pos (f e) i
        S.UnsafePtr p e       -> S.UnsafePtr p (f e)
        S.ADT p e             -> S.ADT p (f e)
        S.Call p ps ident es  -> S.Call p (map f ps) ident (map f es)
        S.Push p e es         -> S.Push p (f e) (map f es)
        S.Pop p e es          -> S.Pop p (f e) (map f es)
        S.Clear p e           -> S.Clear p (f e)
        S.Delete p e1 e2      -> S.Delete p (f e1) (f e2)
        S.Match p e pat       -> S.Match p (f e) (f pat)
        S.Range p me me1 me2  -> S.Range p (fmap f me) (fmap f me1) (fmap f me2)
        _                     -> error $ show expr
        where
            f :: Apply a => a -> a
            f = apply subs


instance Apply S.Pattern where
    apply subs pattern = case pattern of
        S.PatIdent p s         -> S.PatIdent p s
        S.PatLiteral e         -> S.PatLiteral (f e)
        S.PatGuarded p pat e   -> S.PatGuarded p (f pat) (f e)
        S.PatField p s pats    -> S.PatField p s $ map f pats
        S.PatTypeField p t pat -> S.PatTypeField p (f t) (f pat)
        S.PatTuple p pats      -> S.PatTuple p $ map f pats
        S.PatIgnore p          -> S.PatIgnore p
        S.PatArray p pats      -> S.PatArray p $ map f pats
        S.PatAnnotated pat typ -> S.PatAnnotated (f pat) (f typ)
        S.PatNull p            -> S.PatNull p
        _                      -> error $ show pattern
        where
            f :: Apply a => a -> a
            f = apply subs


instance Apply S.Stmt where
    apply subs stmt = case stmt of
        S.Block stmts               -> S.Block $ map f stmts
        S.Return pos mexpr          -> S.Return pos $ fmap f mexpr
        S.Assign pos pat expr       -> S.Assign pos (f pat) (f expr)
        S.Set pos index e           -> S.Set pos (f index) (f e)
        S.While pos cnd blk         -> S.While pos (f cnd) (f blk)
        S.ExprStmt e                -> S.ExprStmt (f e)
        S.Print pos es              -> S.Print pos $ map f es
        S.Data pos symbol typ mexpr -> S.Data pos symbol (f typ) (fmap f mexpr)

        S.FuncDef pos mparam sym params retty block ->
            S.FuncDef pos (fmap f mparam) sym (map f params) (f retty) (f block)

        S.If pos cnd block melse ->
            S.If pos (f cnd) (f block) (fmap f melse)

        S.Typedef _ _ _ -> stmt -- leave this for now
        
        S.Switch pos expr cases ->
            S.Switch pos (f expr) [(f p, f s) | (p, s) <- cases]

        S.For pos expr mpat blk ->
            S.For pos (f expr) (fmap f mpat) (f blk)
        where
            f :: Apply a => a -> a
            f = apply subs
