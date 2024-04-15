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
    | Tuple Type
    | Table Type
    | ADT [Type]
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
        Tuple t           -> "()" ++ show t
        Table t           -> "Table[" ++ show t ++ "]"
        ADT ts            -> "(" ++ intercalate " | " (map show ts) ++ ")"
        TypeApply s []    -> show s
        TypeApply s ts    -> show s ++ "(" ++ intercalate ", " (map show ts) ++ ")"


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

isRecord :: Type -> Bool
isRecord _               = False


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
    U8             -> typ
    I8             -> typ
    I16            -> typ
    I32            -> typ
    I64            -> typ
    F32            -> typ
    F64            -> typ
    Bool           -> typ
    String         -> typ
    Char           -> typ
    Type _         -> typ
    Tuple t        -> Tuple (mapType f t)
    Table t        -> Table (mapType f t)
    TypeApply s ts -> TypeApply s $ map (mapType f) ts
    ADT ts         -> ADT $ map (mapType f) ts
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
    ADT ts         -> return $ Just (ADT ts)
    Tuple t        -> return $ Just (Tuple t)
    Table t        -> return $ Just (Table t)
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
    flattenType =<< case typ of
        TypeApply s [] -> case lookup s args of
            Just x  -> return x
            Nothing -> return typ

        TypeApply s ts -> case lookup s args of
            Just x  -> error (show x)
            Nothing -> TypeApply s <$> mapM (applyTypeArguments argSymbols argTypes) ts

        Tuple t                 -> Tuple <$> applyTypeArguments argSymbols argTypes t
        Table t                 -> Table <$> applyTypeArguments argSymbols argTypes t
        ADT ts                  -> ADT <$> mapM (applyTypeArguments argSymbols argTypes) ts
        _ | isSimple typ        -> return typ
        Void                    -> return typ
        _                       -> error $ "applyTypeArguments: " ++ show typ


typesCouldMatch :: (MonadFail m, TypeDefs m) => [Symbol] -> Type -> Type -> m Bool
typesCouldMatch generics t1 t2 = do
    flat1 <- addTuple =<< flattenType t1
    flat2 <- addTuple =<< flattenType t2
    couldMatch flat1 flat2
    where
        addTuple :: (MonadFail m, TypeDefs m) => Type -> m Type
        addTuple typ = do
            b <- definitelyIgnoresTuples typ
            return $ case typ of
                Tuple t -> Tuple t
                t | b   -> Tuple t
                t       -> t

        -- pure version of function that doesn't worry about flattening types
        couldMatch :: (MonadFail m, TypeDefs m) => Type -> Type -> m Bool
        couldMatch t1 t2 = case (t1, t2) of
            (a, b) | a == b                   -> return True
            (Type _, _)                       -> return True
            (_, Type _)                       -> return True

            (Tuple a, Tuple b)                -> couldMatch a b
            (Table a, Table b)                -> typesCouldMatch generics a b

            -- type variables
            (TypeApply s1 ts1, TypeApply s2 ts2)
                | s1 `elem` generics && s2 `elem` generics -> do
                    bs <- zipWithM (typesCouldMatch generics) ts1 ts2
                    return $ length ts1 == length ts2 && all id bs
            (x, TypeApply s ts) | s `elem` generics -> return True
            (TypeApply s ts, x) | s `elem` generics -> return True

            -- type defs
            (TypeApply s1 ts1, TypeApply s2 ts2) | s1 == s2 -> do
                bs <- zipWithM (typesCouldMatch generics) ts1 ts2
                return $ length ts1 == length ts2 && all id bs


--            (I64, I32) -> return False
--            (TypeApply _ _, I64) -> return False
--            (TypeApply _ _, Bool) -> return False
--            (TypeApply _ _, Tuple _) -> return False
--            (Tuple _, TypeApply _ _) -> return False
--
--            x -> error (show x)

            _ -> return False



-- Determines whether the type will change after a tuple application.
-- For example:
-- i64         -> False because ()i64 == i64
-- ()string    -> False because ()()string == ()string
-- {i64, bool} -> True  because (){i64, bool} != {i64, bool}
definitelyIgnoresTuples :: (MonadFail m, TypeDefs m) => Type -> m Bool
definitelyIgnoresTuples typ = case typ of
    ADT _          -> return True
    Void           -> return True
    t | isSimple t -> return True
    Tuple t        -> return True
    Table t        -> return True
    Type _         -> return False
    TypeApply s ts -> do
        resm <- Map.lookup s <$> getTypeDefs
        case resm of
            Just (ss, t) -> definitelyIgnoresTuples =<< applyTypeArguments ss ts t
            Nothing      -> return False -- must be generic

    _ -> error (show typ)


-- {}i64           -> {i64}
-- {}Person        -> Person
-- {}Vec2f         -> {}Vec2f
-- {}(){i64, bool} -> {i64, bool}
-- {}[]{i64, bool} -> {[]{i64, bool}}
-- ()i64         -> i64
-- ()()string    -> string
-- ()T           -> ()T
-- (){i64, bool} -> (){i64, bool}
flattenType :: (MonadFail m, TypeDefs m) => Type -> m Type
flattenType typ = case typ of
    t | isSimple t -> return typ
    Void           -> return typ
    Type _         -> return typ
    Table t        -> Table <$> flattenType t
    ADT ts         -> ADT <$> mapM flattenType ts
    TypeApply s ts -> TypeApply s <$> mapM flattenType ts

    Tuple t -> do
        t' <- flattenType t
        b <- definitelyIgnoresTuples t'
        return $ if b then t' else Tuple t'

    x -> error (show x)


getTypeFieldIndex :: (MonadFail m, TypeDefs m) => Type -> Type -> m Int
getTypeFieldIndex typ field = do
    base <- baseTypeOf typ
    case base of
        Tuple t   -> do 
            b <- baseTypeOf t
            getTypeFieldIndex b field
        _ -> error (show typ)

