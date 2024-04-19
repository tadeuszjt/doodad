module Apply where

import qualified Data.Map as Map
import Type
import Constraint
import AST
import ASTResolved


applyAST :: [(Type, Type)] -> ASTResolved -> ASTResolved
applyAST subs ast = ast { funcDefs = Map.map (applyFuncBody subs) (funcDefs ast) }

applyFuncBody :: [(Type, Type)] -> FuncBody -> FuncBody
applyFuncBody subs body = body {
    funcParams = map (applyParam subs) (funcParams body),
    funcStmt = applyStmt subs (funcStmt body),
    funcArgs = map (applyParam subs) (funcArgs body),
    funcRetty = applyRetty subs (funcRetty body)
    }


applyRetty :: [(Type, Type)] -> Retty -> Retty
applyRetty subs (VoidRetty)  = VoidRetty
applyRetty subs (Retty t)    = Retty (applyType subs t)
applyRetty subs (RefRetty t) = RefRetty (applyType subs t)


applyParam :: [(Type, Type)] -> Param -> Param
applyParam subs (Param pos symbol typ) = Param pos symbol (applyType subs typ)
applyParam subs (RefParam pos symbol typ) = RefParam pos symbol (applyType subs typ)


applyStmt :: [(Type, Type)] -> Stmt -> Stmt
applyStmt subs stmt = case stmt of
    Return pos mexpr -> Return pos (fmap applyEx mexpr)
    Block stmts      -> Block (map applySt stmts)
    If pos cnd blk mblk  -> If pos (applyEx cnd) (applySt blk) (fmap applySt mblk)
    ExprStmt expr -> ExprStmt (applyEx expr)
    EmbedC pos s -> EmbedC pos s
    Data pos symbol typ mexpr -> Data pos symbol (applyTy typ) (fmap applyEx mexpr)
    SetOp pos op expr1 expr2 -> SetOp pos op (applyEx expr1) (applyEx expr2)
    Let pos pattern mexpr mblk -> Let pos (applyPat pattern) (fmap applyEx mexpr) (fmap applySt mblk)
    Switch pos expr cases -> Switch pos (applyEx expr) $ map (\(p, st) -> (applyPat p, applySt st)) cases
    While pos expr blk -> While pos (applyEx expr) (applySt blk)
    For pos expr mpat blk -> For pos (applyEx expr) (fmap applyPat mpat) (applySt blk)
    x -> error (show x)
    where
        applySt = applyStmt subs
        applyEx = applyExpr subs
        applyTy = applyType subs
        applyPat = applyPattern subs


applyExpr :: [(Type, Type)] -> Expr -> Expr
applyExpr subs expression = case expression of
    AExpr typ expr -> AExpr (applyTy typ) (applyEx expr)
    Match pos expr pattern -> Match pos (applyEx expr) (applyPat pattern)
    Ident pos symbol -> Ident pos symbol
    AST.Bool pos b -> AST.Bool pos b
    AST.Char pos c -> AST.Char pos c
    Call pos mparam symbol args -> Call pos (fmap applyEx mparam) symbol (map applyEx args)
    Builtin pos symbol args -> Builtin pos symbol (map applyEx args)
    Infix pos op expr1 expr2 -> Infix pos op (applyEx expr1) (applyEx expr2)
    Prefix pos op expr -> Prefix pos op (applyEx expr)
    AST.Int pos n -> AST.Int pos n
    AST.Float pos n -> AST.Float pos n
    AST.String pos s -> AST.String pos s
    Field pos expr symbol -> Field pos (applyEx expr) symbol
    AST.Tuple pos exprs -> AST.Tuple pos (map applyEx exprs)
    Construct pos symbol exprs -> Construct pos symbol (map applyEx exprs)
    AST.Reference pos expr -> AST.Reference pos (applyEx expr)
    x -> error (show x)
    where
        applyEx = applyExpr subs
        applyPat = applyPattern subs
        applyTy = applyType subs


applyType :: [(Type, Type)] -> Type -> Type
applyType subs = mapType (f subs)
    where
        f :: [(Type, Type)] -> Type -> Type
        f []          z = z
        f ((x, u):xs) z = f xs (if z == x then u else z)



applyPattern :: [(Type, Type)] -> Pattern -> Pattern
applyPattern subs pattern = case pattern of
    PatAnnotated pat typ -> PatAnnotated (applyPat pat) (applyTy typ)
    PatField pos symbol pats -> PatField pos symbol (map applyPat pats)
    PatIgnore pos -> PatIgnore pos
    PatIdent pos symbol -> PatIdent pos symbol
    PatGuarded pos pat expr -> PatGuarded pos (applyPat pat) (applyEx expr)
    PatTuple pos pats -> PatTuple pos (map applyPat pats)
    PatLiteral expr -> PatLiteral (applyEx expr)
    x -> error (show x)
    where
        applyEx = applyExpr subs
        applyPat = applyPattern subs
        applyTy = applyType subs



applyConstraint :: [(Type, Type)] -> Constraint -> Constraint
applyConstraint subs constraint = case constraint of
    ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
    ConsBase t1 t2         -> ConsBase (rf t1) (rf t2)
    ConsAdtField t i ts    -> ConsAdtField (rf t) i (map rf ts)
    ConsField  t1 s t2     -> ConsField (rf t1) s (rf t2)
    ConsForExpr t1 t2      -> ConsForExpr (rf t1) (rf t2)
    ConsCall t1 s t2s      -> ConsCall (rf t1) s (map rf t2s)
    where
        rf = applyType subs


