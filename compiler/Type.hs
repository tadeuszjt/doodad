{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where

import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map (Map, (!), member, lookup)
import Data.List
import Symbol

type TypeDefsMap = Map.Map Symbol ([Symbol], Type)

class Monad m => TypeDefs m where getTypeDefs :: m TypeDefsMap

newtype TypeDefsMonad a = TypeDefsMonad { getState :: State TypeDefsMap a }
    deriving (Applicative, Functor, Monad, MonadState TypeDefsMap)

instance TypeDefs TypeDefsMonad where getTypeDefs = get

runTypeDefsMonad :: TypeDefsMap -> TypeDefsMonad a -> a
runTypeDefsMonad typeDefs f = evalState (getState f) typeDefs


class Typeof a where typeof :: a -> Type
instance Typeof Type where typeof = id

data Type
    = Type Int
    | Void
    | U8
    | I8                     
    | I16                    
    | I32                    
    | I64                    
    | F32                    
    | F64                    
    | Bool                   
    | Char                   
    | String
    | TypeApply Symbol [Type]
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        Type id           -> "t" ++ show id
        Void              -> "void"
        U8                -> "U8"
        I8                -> "I8"
        I16               -> "I16"
        I32               -> "I32"
        I64               -> "I64"
        F32               -> "F32"
        F64               -> "F64"
        Bool              -> "Bool"
        Char              -> "Char"
        String            -> "String"
        TypeApply s []    -> show s
        TypeApply s ts    -> show s ++ "[" ++ intercalate ", " (map show ts) ++ "]"


isInt :: Type -> Bool
isInt typ = case typ of
    U8 -> True
    I8 -> True
    I16 -> True
    I32 -> True
    I64 -> True
    _ -> False

isFloat :: Type -> Bool
isFloat typ = case typ of
    F32 -> True
    F64 -> True
    _   -> False


isSimple :: Type -> Bool
isSimple typ = case typ of
    U8 -> True
    I8 -> True
    I16 -> True
    I32 -> True
    I64 -> True
    F32 -> True
    F64 -> True
    Bool -> True
    String -> True
    Char -> True
    _ -> False

isIntegral x = isInt x || x == Char

mapType :: (Type -> Type) -> Type -> Type
mapType f typ = f $ case typ of
    x | isSimple x -> typ
    Type _         -> typ
    TypeApply s ts -> TypeApply s $ map (mapType f) ts
    Void           -> typ
    _ -> error (show typ)


baseTypeOf :: (MonadFail m, TypeDefs m, Typeof a) => a -> m Type
baseTypeOf a = do
    resm <- baseTypeOfm a
    case resm of
        Nothing -> error "baseTypeOf"
        Just x  -> return x


baseTypeOfm :: (MonadFail m, TypeDefs m, Typeof a) => a -> m (Maybe Type)
baseTypeOfm a = case typeof a of
    Type x         -> return Nothing
    t | isSimple t -> return $ Just t
    TypeApply (Sym "Sum") ts  -> return $ Just $ typeof a
    TypeApply (Sym "Tuple") t -> return $ Just $ typeof a
    TypeApply (Sym "Table") t -> return $ Just $ typeof a
    Void           -> return $ Just Void

    TypeApply symbol ts -> do
        resm <- Map.lookup symbol <$> getTypeDefs
        case resm of
            Nothing -> return Nothing
            Just (ss, t) -> baseTypeOfm =<< applyTypeArguments ss ts t

    x -> error (show x)


applyTypeArguments :: (MonadFail m, TypeDefs m) => [Symbol] -> [Type] -> Type -> m Type
applyTypeArguments argSymbols argTypes typ = do
    unless (length argSymbols == length argTypes) (fail $ "invalid arguments: " ++ show typ)
    let args = zip argSymbols argTypes
    case typ of
        TypeApply s [] -> case lookup s args of
            Just x  -> return x
            Nothing -> return typ

        TypeApply s ts -> case lookup s args of
            Just x  -> error (show x)
            Nothing -> TypeApply s <$> mapM (applyTypeArguments argSymbols argTypes) ts

        _ | isSimple typ -> return typ
        Void             -> return typ
        _                -> error $ "applyTypeArguments: " ++ show typ


typesCouldMatch :: (MonadFail m, TypeDefs m) => [Symbol] -> Type -> Type -> m Bool
typesCouldMatch generics t1 t2 = couldMatch t1 t2
    where
        couldMatch :: (MonadFail m, TypeDefs m) => Type -> Type -> m Bool
        couldMatch t1 t2 = case (t1, t2) of
            (a, b) | a == b            -> return True
            (Type _, _)                -> return True
            (_, Type _)                -> return True

            (TypeApply s1 ts1, TypeApply s2 ts2)
                | (s1 `elem` generics && s2 `elem` generics) || (s1 == s2) -> do
                    bs <- zipWithM couldMatch ts1 ts2
                    return $ length ts1 == length ts2 && all id bs

            (x, TypeApply s ts) | s `elem` generics -> return True
            (TypeApply s ts, x) | s `elem` generics -> return True

            _ -> return False
