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
    | Slice
    | Tuple
    | Table
    | Sum
    | Array
    | Func
    | Size Int
    | TypeDef Symbol
    | Apply Type [Type]
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        Type id       -> "t" ++ show id
        Void          -> "void"
        U8            -> "U8"
        I8            -> "I8"
        I16           -> "I16"
        I32           -> "I32"
        I64           -> "I64"
        F32           -> "F32"
        F64           -> "F64"
        Bool          -> "Bool"
        Char          -> "Char"
        Tuple         -> "Tuple"
        Table         -> "Table"
        Sum           -> "Sum"
        Slice         -> "Slice"
        Func          -> "Func"
        TypeDef s     -> prettySymbol s

        Apply t1 [t2] -> show t2 ++ "." ++ show t1
        Apply t ts    -> show t ++ "{" ++ intercalate ", " (map show ts) ++ "}"
        Size n        -> show n
        x -> error ""


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
    Char -> True
    _ -> False

isIntegral x = isInt x || x == Char


isGeneric :: Type -> Bool
isGeneric (TypeDef s) = "type" `elem` symStr s
isGeneric _           = False


mapType :: (Type -> Type) -> Type -> Type
mapType f typ = f $ case typ of
    Apply t ts -> Apply (mapType f t) $ map (mapType f) ts
    _          -> typ


baseTypeOf :: (MonadFail m, TypeDefs m, Typeof a) => a -> m Type
baseTypeOf a = do
    resm <- baseTypeOfm a
    case resm of
        Nothing -> fail ("baseTypeOfm returned Nothing: " ++ show (typeof a))
        Just x  -> return x


baseTypeOfm :: (MonadFail m, TypeDefs m, Typeof a) => a -> m (Maybe Type)
baseTypeOfm a = case typeof a of
    Type _ -> return Nothing

    Apply (TypeDef s) ts -> do
        Just (ss, t) <- Map.lookup s <$> getTypeDefs
        baseTypeOfm =<< applyTypeArguments ss ts t

    TypeDef s -> do
        Just ([], t) <- Map.lookup s <$> getTypeDefs
        baseTypeOfm t

    _ -> return $ Just (typeof a)


applyTypeArguments :: (MonadFail m, TypeDefs m) => [Symbol] -> [Type] -> Type -> m Type
applyTypeArguments argSymbols argTypes typ = do
    unless (length argSymbols == length argTypes) (fail $ "invalid arguments: " ++ show typ)
    case typ of
        TypeDef s -> case elemIndex s argSymbols of
            Just x -> return (argTypes !! x)
            Nothing -> return (TypeDef s)

        Apply t ts -> do
            ts' <- mapM (applyTypeArguments argSymbols argTypes) (t : ts)
            return $ Apply (head ts') (tail ts')

        _ -> return typ


typesCouldMatch :: Monad m => Type -> Type -> m Bool
typesCouldMatch t1 t2 = case (t1, t2) of
    (a, b) | a == b            -> return True
    (Type _, _)                -> return True
    (_, Type _)                -> return True

    (TypeDef s, _) | isGeneric t1 -> return True
    (_, TypeDef s) | isGeneric t2 -> return True

    (Apply t1 ts1, Apply t2 ts2)
        | length ts1 == length ts2 ->
            all id <$> zipWithM typesCouldMatch (t1 : ts1) (t2 : ts2)

    (TypeDef s1, TypeDef s2) | symbolsCouldMatch s1 s2 ->
        return True

    _ -> return False


typeFullyResolved :: Type -> Bool
typeFullyResolved typ = case typ of
    Type _         -> False
    x | isGeneric x -> False
    Apply t ts     -> all typeFullyResolved (t : ts)
    _              -> True
