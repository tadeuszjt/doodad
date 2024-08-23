module Apply where

import Type
import Constraint
import AST


applyFunc :: [(Type, Type)] -> Func -> Func
applyFunc subs func = func
    { funcStmt = applyStmt subs (funcStmt func)
    , funcArgs = map (applyParam subs) (funcArgs func)
    , funcRetty = applyRetty subs (funcRetty func)
    , funcSymbol = funcSymbol func
    }


applyRetty :: [(Type, Type)] -> Retty -> Retty
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
    EmbedC pos m s -> EmbedC pos m s
    Data pos symbol typ mexpr -> Data pos symbol (applyTy typ) (fmap applyEx mexpr)
    Let pos pattern mexpr Nothing -> Let pos (applyPat pattern) (fmap applyEx mexpr) Nothing
    While pos expr blk -> While pos (applyEx expr) (applySt blk)
    Assign pos symbol expr -> Assign pos symbol (applyEx expr)
    Acquires pos generics typ args isRef stmt -> Acquires pos generics typ args isRef (applySt stmt)
    Derives pos generics symbol symbols -> Derives pos generics symbol symbols
    x -> error "invalid statement"
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
    Call pos typ exprs -> Call pos (applyTy typ) (map applyEx exprs)
    AST.Int pos n -> AST.Int pos n
    AST.Float pos n -> AST.Float pos n
    AST.String pos s -> AST.String pos s
    AST.Reference pos expr -> AST.Reference pos (applyEx expr)
    AST.Array pos exprs -> AST.Array pos (map applyEx exprs)
    x -> error (show x)
    where
        applyEx = applyExpr subs
        applyPat = applyPattern subs
        applyTy = applyType subs


applyPattern :: [(Type, Type)] -> Pattern -> Pattern
applyPattern subs pattern = case pattern of
    PatAnnotated pat typ -> PatAnnotated (applyPattern subs pat) (applyType subs typ)
    PatIdent pos symbol -> PatIdent pos symbol
    x -> error (show x)


applyConstraint :: [(Type, Type)] -> Constraint -> Constraint
applyConstraint subs constraint = case constraint of
    ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
    ConsField  t1 s t2     -> ConsField (rf t1) s (rf t2)
    ConsSlice t1 t2        -> ConsSlice (rf t1) (rf t2)
    ConsDefault t1 t2      -> ConsDefault (rf t1) (rf t2)
    where
        rf = applyType subs


