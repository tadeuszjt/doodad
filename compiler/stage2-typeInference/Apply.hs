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

-- constraint:   (t1, t2) or (x, y)
-- substitution: (id, t) or (x, u)
substitute :: Type -> Type -> Type -> Type
substitute u x typ = case typ of
    _ | typ == x         -> u  -- this one replaces regardless of type
    --Type _ | typ == x    -> u
    Type _               -> typ
    Void                 -> typ
    _ | isSimple typ     -> typ
    TypeApply s ts       -> TypeApply s $ map (substitute u x) ts
    Record ts            -> Record $ map (substitute u x) ts
    Tuple t              -> Tuple (substitute u x t)
    Table t              -> Table (substitute u x t)
    Range t              -> Range (substitute u x t)
    _                    -> error (show typ)


applySubs :: (Apply a, BoM s m) => [(Type, Type)] -> a -> m a
applySubs subs a = apply (\t -> foldr (\(x, u) z -> substitute u x z) t subs) a


mapper :: BoM s m => (Type -> Type) -> Elem -> m (Maybe Elem)
mapper f elem = case elem of
    ElemStmt (S.Const _ _ _)     -> return Nothing
    ElemStmt (S.Typedef _ _ _ _) -> return Nothing
    ElemStmt _                   -> return (Just elem)
    ElemExpr _                   -> return (Just elem)
    ElemPattern _                -> return (Just elem)
    ElemType typ                 -> return $ Just $ ElemType (f typ)
    _ -> error (show elem)

-- Apply represents taking a function and applying it to all types in an object.
class Apply a             where apply :: BoM s m => (Type -> Type) -> a -> m a
instance Apply FuncBody   where apply f body = mapFuncBody (mapper f) body
instance Apply FuncHeader where apply f header = mapFuncHeader (mapper f) header
instance Apply S.Stmt     where apply f stmt = mapStmt (mapper f) stmt
instance Apply S.Param    where apply f (S.Param p n t) = return $ S.Param p n (f t)
            
instance Apply ASTResolved where
    apply f ast = do
        funcDefs' <- mapM (apply f) (funcDefs ast)
        return $ ast { funcDefs = funcDefs' }

instance Apply Constraint where
    apply f constraint = case constraint of
        ConsEq t1 t2         -> return $ ConsEq (f t1) (f t2)
        ConsBase t1 t2       -> return $ ConsBase (f t1) (f t2)
        ConsMember t1 i t2   -> return $ ConsMember (f t1) i (f t2)
        ConsElem t1 t2       -> return $ ConsElem (f t1) (f t2)
        ConsSubscript t1 t2  -> return $ ConsSubscript (f t1) (f t2)
        ConsField t1 i t2    -> return $ ConsField (f t1) i (f t2)
        ConsAdtField t1 i j t2 -> return $ ConsAdtField (f t1) i j (f t2)
        ConsTuple t1 ts      -> return $ ConsTuple (f t1) (map f ts)
        ConsRecordAccess t1 t2 -> return $ ConsRecordAccess (f t1) (f t2)


