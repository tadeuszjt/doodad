module Symbol where

import AST as S
import Type as T


collapseAstSymbols :: AST -> AST
collapseAstSymbols ast =
    ast { astStmts = map (mapSymbol f) (astStmts ast) }
    where
        f :: Symbol -> Symbol
        f s@(Sym sym) = s
        f s@(SymQualified "c" sym) = s
        f s@(SymQualified mod sym) = error $ "No SymQualified allowed: " ++ show s
        f s@(SymResolved mod sym i) = Sym $ "bo_" ++ mod ++ "_" ++ sym ++ "_" ++ show i


class MapSymbol a where
    mapSymbol :: (Symbol -> Symbol) -> a -> a

instance MapSymbol Symbol where
    mapSymbol f sym = f sym

instance MapSymbol Type where
    mapSymbol f t = case t of
        T.Typedef s -> T.Typedef (mapSymbol f s)
        T.Tuple ts  -> T.Tuple $ map (mapSymbol f) ts
        Array i t -> Array i (mapSymbol f t)
        T.Table ts  -> T.Table $ map (mapSymbol f) ts
        Func ts t -> Func (map (mapSymbol f) ts) (mapSymbol f t)
        ADT ts    -> ADT $ map (mapSymbol f) ts
        _         -> t

    
instance MapSymbol Param where
    mapSymbol f (Param p s t) = Param p (mapSymbol f s) (mapSymbol f t)

instance MapSymbol Expr where
    mapSymbol f expr = case expr of
        AExpr t e                -> AExpr (mapSymbol f t) (mapSymbol f e)
        Infix pos op expr1 expr2 -> Infix pos op (mapSymbol f expr1) (mapSymbol f expr2)
        S.Tuple pos exprs        -> S.Tuple pos $ map (mapSymbol f) exprs
        Ident pos sym            -> Ident pos (mapSymbol f sym)
        S.Char  pos c            -> expr
        Int   pos n              -> expr
        Prefix pos op expr1      -> Prefix pos op (mapSymbol f expr1)
        Call  pos sym exprs      -> Call pos (mapSymbol f sym) $ map (mapSymbol f) exprs
        --Call  pos sym exprs      -> Call pos sym $ map (mapSymbol f) exprs
        Conv  pos t exprs        -> Conv pos (mapSymbol f t) $ map (mapSymbol f) exprs
        Copy  pos e              -> Copy pos (mapSymbol f e)
        Len   pos e              -> Len  pos (mapSymbol f e)
        S.Bool  pos b            -> expr
        Subscript pos e1 e2      -> Subscript pos (mapSymbol f e1) (mapSymbol f e2)
        String pos s             -> expr
        Member pos e s           -> Member pos (mapSymbol f e) s
        S.Float pos f            -> expr
        _                          -> error $ show expr

instance MapSymbol Condition where
    mapSymbol f cnd = case cnd of
        CondExpr expr -> CondExpr (mapSymbol f expr)
        _             -> error $ show cnd

instance MapSymbol Pattern where
    mapSymbol f pattern = case pattern of
        PatIdent p s       -> PatIdent p (mapSymbol f s)
        PatLiteral e       -> PatLiteral (mapSymbol f e)
        PatGuarded p pat e -> PatGuarded p (mapSymbol f pat) (mapSymbol f e)
        PatField p s pat   -> PatField p (mapSymbol f s) (mapSymbol f pat)
        PatTuple p pats    -> PatTuple p $ map (mapSymbol f) pats
        _                  -> error $ show pattern

instance MapSymbol Append where
    mapSymbol f app = case app of
        AppendTable p ap e -> AppendTable p (mapSymbol f ap) (mapSymbol f e)
        AppendIndex index   -> AppendIndex (mapSymbol f index)

instance MapSymbol Index where
    mapSymbol f index = case index of
        IndIdent p sym -> IndIdent p (mapSymbol f sym)
        IndArray p ind e -> IndArray p (mapSymbol f ind) (mapSymbol f e)
        _ -> error $ show index

instance MapSymbol AnnoType where
    mapSymbol f anno = case anno of
        AnnoType t -> AnnoType (mapSymbol f t)
        AnnoTuple xs -> AnnoTuple $ map (\(s, t) -> (s, mapSymbol f t)) xs
        AnnoADT xs -> AnnoADT $ map (\(s, t) -> (mapSymbol f s, mapSymbol f t)) xs

instance MapSymbol Stmt where
    mapSymbol f stmt = case stmt of
        Block stmts             -> Block $ map (mapSymbol f) stmts
        Return pos mexpr        -> Return pos $ fmap (mapSymbol f) mexpr
        Assign pos pat expr     -> Assign pos (mapSymbol f pat) (mapSymbol f expr)
        AppendStmt app          -> AppendStmt (mapSymbol f app)
        Set pos index e         -> Set pos (mapSymbol f index) (mapSymbol f e)
        While pos cnd blk       -> While pos (mapSymbol f cnd) (mapSymbol f blk)
        CallStmt pos sym es     -> error (show stmt) --CallStmt pos (mapSymbol f sym) $ map (mapSymbol f) es
        Print pos es            -> Print pos $ map (mapSymbol f) es

        FuncDef pos sym params retty block ->
            --FuncDef pos (mapSymbol f sym) (map (mapSymbol f) params) (mapSymbol f retty) (mapSymbol f block)
            FuncDef pos sym (map (mapSymbol f) params) (mapSymbol f retty) (mapSymbol f block)

        If pos cnd block melse ->
            If pos (mapSymbol f cnd) (mapSymbol f block) $ fmap (mapSymbol f) melse

        S.Typedef pos symbol anno -> S.Typedef pos (mapSymbol f symbol) (mapSymbol f anno)
        
        Switch pos expr cases ->
            Switch pos (mapSymbol f expr) [(mapSymbol f p, mapSymbol f s) | (p, s) <- cases]
            

instance MapSymbol AST where
    mapSymbol f ast = ast { astStmts = map (mapSymbol f) (astStmts ast) }
