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
    UnsafePtr        -> UnsafePtr
    _                -> error (show typ)
    where
        subAdtField :: AdtField -> AdtField
        subAdtField field = case field of
            FieldNull -> FieldNull
            FieldType t -> FieldType (substitute u x t)
            FieldCtor ts -> FieldCtor $ map (substitute u x) ts


applySubs :: Apply a => [(Int, Type)] -> a -> a
applySubs subs a = apply (\t -> foldr (\(x, u) z -> substitute u x z) t subs) a

-- Apply represents taking a function and applying it to all types in an object.
class Apply a where
    apply :: (Type -> Type) -> a -> a

            
instance Apply ResolvedAst where
    apply f ast = ast { funcDefs = Map.map (apply f) (funcDefs ast) }


instance Apply FuncBody where
    apply f body = FuncBody
        { funcParams = map (apply f) (funcParams body)
        , funcArgs   = map (apply f) (funcArgs body)
        , funcRetty  = (apply f) (funcRetty body)
        , funcStmts  = map (apply f) (funcStmts body)
        }

instance Apply Type where
    apply f t = (f t)
    
instance Apply Constraint where
    apply f constraint = case constraint of
        ConsEq t1 t2         -> ConsEq (applyF t1) (applyF t2)
        ConsBase t1 t2       -> ConsBase (applyF t1) (applyF t2)
        ConsMember t1 i t2   -> ConsMember (applyF t1) i (applyF t2)
        ConsSubscript t1 t2  -> ConsSubscript (applyF t1) (applyF t2)
        ConsField t1 i t2    -> ConsField (applyF t1) i (applyF t2)
        ConsAdtMem t1 i j t2 -> ConsAdtMem (applyF t1) i j (applyF t2)
        where
            applyF :: Apply a => a -> a
            applyF = apply f

instance Apply S.Param where
    apply f (S.Param p n t) = S.Param p n (apply f t)

instance Apply S.Expr where
    apply f expr = case expr of
        S.AExpr t e           -> S.AExpr (applyF t) (applyF e)
        S.Infix pos op e1 e2  -> S.Infix pos op (applyF e1) (applyF e2)
        S.Tuple pos es        -> S.Tuple pos (map applyF es)
        S.Ident pos sym       -> S.Ident pos sym
        S.Char  pos c         -> S.Char pos c
        S.Int   pos n         -> S.Int pos n
        S.Null  pos           -> S.Null pos
        S.Prefix pos op e1    -> S.Prefix pos op (applyF e1)
        S.Conv  pos t es      -> S.Conv pos (applyF t) (map applyF es)
        S.Bool  pos b         -> S.Bool pos b
        S.Subscript pos e1 e2 -> S.Subscript pos (applyF e1) (applyF e2)
        S.String pos s        -> S.String pos s
        S.Field pos e s       -> S.Field pos (applyF e) s
        S.Float pos f         -> S.Float pos f
        S.Initialiser pos es  -> S.Initialiser pos (map applyF es)
        S.ADT p e             -> S.ADT p (applyF e)
        S.Call p ps ident es  -> S.Call p (map applyF ps) ident (map applyF es)
        S.Builtin p ps ident es  -> S.Builtin p (map applyF ps) ident (map applyF es)
        S.Match p e pat       -> S.Match p (applyF e) (applyF pat)
        S.Range p me me1 me2  -> S.Range p (fmap applyF me) (fmap applyF me1) (fmap applyF me2)
        _                     -> error $ show expr
        where
            applyF :: Apply a => a -> a
            applyF = apply f


instance Apply S.Pattern where
    apply f pattern = case pattern of
        S.PatIdent p s         -> S.PatIdent p s
        S.PatLiteral e         -> S.PatLiteral (applyF e)
        S.PatGuarded p pat e   -> S.PatGuarded p (applyF pat) (applyF e)
        S.PatField p s pats    -> S.PatField p s $ map applyF pats
        S.PatTypeField p t pat -> S.PatTypeField p (applyF t) (applyF pat)
        S.PatTuple p pats      -> S.PatTuple p $ map applyF pats
        S.PatIgnore p          -> S.PatIgnore p
        S.PatArray p patss     -> S.PatArray p $ map (map applyF) patss
        S.PatAnnotated pat typ -> S.PatAnnotated (applyF pat) (applyF typ)
        S.PatNull p            -> S.PatNull p
        _                      -> error $ show pattern
        where
            applyF :: Apply a => a -> a
            applyF = apply f


instance Apply S.Stmt where
    apply f stmt = case stmt of
        S.Block stmts               -> S.Block $ map applyF stmts
        S.Return pos mexpr          -> S.Return pos $ fmap applyF mexpr
        S.Assign pos pat expr       -> S.Assign pos (applyF pat) (applyF expr)
        S.Set pos index e           -> S.Set pos (applyF index) (applyF e)
        S.While pos cnd blk         -> S.While pos (applyF cnd) (applyF blk)
        S.ExprStmt e                -> S.ExprStmt (applyF e)
        S.Print pos es              -> S.Print pos $ map applyF es
        S.Data pos symbol typ mexpr -> S.Data pos symbol (applyF typ) (fmap applyF mexpr)

        S.FuncDef pos mparam sym params retty block ->
            S.FuncDef pos (fmap applyF mparam) sym (map applyF params) (applyF retty) (applyF block)

        S.If pos cnd block melse ->
            S.If pos (applyF cnd) (applyF block) (fmap applyF melse)

        S.Typedef _ _ _ -> stmt -- leave this for now
        
        S.Switch pos expr cases ->
            S.Switch pos (applyF expr) [(applyF p, applyF s) | (p, s) <- cases]

        S.For pos expr mpat blk ->
            S.For pos (applyF expr) (fmap applyF mpat) (applyF blk)
        where
            applyF :: Apply a => a -> a
            applyF = apply f 