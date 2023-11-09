module Apply where

import Control.Monad
import Type
import qualified AST as S
import Constraint
import qualified Data.Map as Map
import qualified SymTab
import qualified Resolve
import ASTResolved
import ASTMapper
import Monad


applySubs :: (Apply a) => [(Type, Type)] -> a -> DoM s a
applySubs subs a = apply (f subs) a
    where
        f :: [(Type, Type)] -> Type -> Type
        f []          z = z
        f ((x, u):xs) z = f xs (if z == x then u else z)


mapper :: (Type -> Type) -> Elem -> DoM s Elem
mapper f elem = case elem of
    ElemType typ                 -> return $ ElemType (f typ)
    ElemStmt _                   -> return elem
    ElemExpr _                   -> return elem
    ElemPattern _                -> return elem
    _ -> error (show elem)

-- Apply represents taking a function and applying it to all types in an object.
class Apply a             where apply :: (Type -> Type) -> a -> DoM s a
instance Apply FuncBody   where apply f body    = mapFuncBodyM (mapper f) body
instance Apply FuncHeader where apply f header  = mapFuncHeaderM (mapper f) header
instance Apply S.Stmt     where apply f stmt    = mapStmtM (mapper f) stmt
            
instance Apply ASTResolved where
    apply f ast = do
        funcDefs' <- mapM (apply f) (funcDefs ast)
        return $ ast { funcDefs = funcDefs' }

-- TODO this is broken, needs to recurse
instance Apply Constraint where
    apply f constraint = case constraint of
        ConsEq t1 t2           -> return $ ConsEq (rf t1) (rf t2)
        ConsBase t1 t2         -> return $ ConsBase (rf t1) (rf t2)
        ConsSubscript t1 t2    -> return $ ConsSubscript (rf t1) (rf t2)
        ConsAdtField t1 i j t2 -> return $ ConsAdtField (rf t1) i j (rf t2)
        ConsTuple t1 ts        -> return $ ConsTuple (rf t1) (map rf ts)
        ConsRecordAccess t1 t2 -> return $ ConsRecordAccess (rf t1) (rf t2)
        ConsSpecial t1 t2      -> return $ ConsSpecial (rf t1) (rf t2)
        ConsField  t1 s t2     -> return $ ConsField (rf t1) s (rf t2)
        where
            rf = mapType f


