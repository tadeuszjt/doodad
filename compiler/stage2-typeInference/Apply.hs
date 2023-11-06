{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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


applySubs :: (Apply a, BoM s m) => [(Type, Type)] -> a -> m a
applySubs subs a = apply (\t -> foldr (\(x, u) z -> if z == x then u else z) t subs) a


mapper :: BoM s m => (Type -> Type) -> Elem -> m (Maybe Elem)
mapper f elem = case elem of
    ElemType typ                 -> return $ Just $ ElemType (f typ)
    ElemStmt _                   -> return (Just elem)
    ElemExpr _                   -> return (Just elem)
    ElemPattern _                -> return (Just elem)
    ElemStmt (S.Const _ _ _)     -> return Nothing
    ElemStmt (S.Typedef _ _ _ _) -> return Nothing
    _ -> error (show elem)

-- Apply represents taking a function and applying it to all types in an object.
class Apply a             where apply :: BoM s m => (Type -> Type) -> a -> m a
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


