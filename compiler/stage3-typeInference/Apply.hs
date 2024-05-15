module Apply where

import qualified Data.Map as Map
import Type
import Constraint
import AST
import ASTResolved


applyFunc :: [(Type, Type)] -> Func -> Func
applyFunc subs func = func
    { funcStmt = applyStmt subs (funcStmt func)
    , funcHeader = applyFuncHeader subs (funcHeader func)
    }


applyFuncHeader :: [(Type, Type)] -> FuncHeader -> FuncHeader
applyFuncHeader subs header = header
    { funcArgs = map (applyParam subs) (funcArgs header)
    , funcRetty = applyRetty subs (funcRetty header)
    , funcSymbol = funcSymbol header
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
    EmbedC pos s -> EmbedC pos s
    Data pos symbol typ mexpr -> Data pos symbol (applyTy typ) (fmap applyEx mexpr)
    Let pos pattern Nothing Nothing -> Let pos (applyPat pattern) Nothing Nothing
    While pos expr blk -> While pos (applyEx expr) (applySt blk)
    Assign pos symbol expr -> Assign pos symbol (applyEx expr)
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
    Call pos symbol args -> Call pos symbol (map applyEx args)
    AST.Int pos n -> AST.Int pos n
    AST.Float pos n -> AST.Float pos n
    AST.String pos s -> AST.String pos s
    Field pos expr symbol -> Field pos (applyEx expr) symbol
    AST.Reference pos expr -> AST.Reference pos (applyEx expr)
    AST.Array pos exprs -> AST.Array pos (map applyEx exprs)
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


