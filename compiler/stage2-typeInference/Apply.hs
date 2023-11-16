module Apply where

import Control.Monad
import Type
import Constraint
import qualified Data.Map as Map
import qualified SymTab
import qualified Resolve
import AST
import ASTResolved
import ASTMapper
import Monad


applySubs :: (Apply a) => [(Type, Type)] -> a -> DoM s a
applySubs subs a = apply (f subs) a
    where
        f :: [(Type, Type)] -> Type -> Type
        f []          z = z
        f ((x, u):xs) z = f xs (if z == x then u else z)


applySubs2 :: (Apply a) => [(Type, Type)] -> a -> DoM s a
applySubs2 subs a = apply (f subs) a
    where
        f :: [(Type, Type)] -> Type -> Type
        f []          z = z
        f ((x, u):xs) z = f xs (if z == x then u else z)


mapper :: (Type -> Type) -> Elem -> DoM s Elem
mapper f (ElemType typ) = return $ ElemType (f typ)
mapper f element        = return element


-- Apply represents taking a function and applying it to all types in an object.
class Apply a             where apply :: (Type -> Type) -> a -> DoM s a
instance Apply FuncBody   where apply f body    = mapFuncBodyM (mapper f) body
instance Apply FuncHeader where apply f header  = mapFuncHeaderM (mapper f) header
instance Apply Stmt       where apply f stmt    = mapStmtM (mapper f) stmt
            
instance Apply ASTResolved where
    apply f ast = do
        funcDefs' <- mapM (apply f) (funcDefs ast)
        return $ ast { funcDefs = funcDefs' }

-- TODO this is broken, needs to recurse
instance Apply Constraint where
    apply f constraint = return $ case constraint of
        ConsEq t1 t2           -> ConsEq (rf t1) (rf t2)
        ConsBase t1 t2         -> ConsBase (rf t1) (rf t2)
        ConsSubscript t1 t2    -> ConsSubscript (rf t1) (rf t2)
        ConsAdtField t i ts    -> ConsAdtField (rf t) i (map rf ts)
        ConsTuple t1 ts        -> ConsTuple (rf t1) (map rf ts)
        ConsRecord t1 ts       -> ConsRecord (rf t1) (map rf ts)
        ConsRecordAccess t1 t2 -> ConsRecordAccess (rf t1) (rf t2)
        ConsSpecial t1 t2      -> ConsSpecial (rf t1) (rf t2)
        ConsField  t1 s t2     -> ConsField (rf t1) s (rf t2)
        where
            rf = mapType f


