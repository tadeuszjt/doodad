{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
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
    | Range Type
    | Record [Type]
    | Tuple Type
    | Table Type
    | ADT [Type]
    | TypeApply Symbol [Type]
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        Type id           -> "t" ++ show id
        Void              -> "void"
        U8                -> "u8"
        I8                -> "i8"
        I16               -> "i16"
        I32               -> "i32"
        I64               -> "i64"
        F32               -> "f32"
        F64               -> "f64"
        Bool              -> "bool"
        Char              -> "char"
        String            -> "string"
        Range t           -> "[..]" ++ show t
        Record ts         -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Tuple (Record ts) -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Tuple t           -> "()" ++ show t
        Table t           -> "[]" ++ show t
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
        Record ts      -> Record $ map (mapType f) ts
        Tuple t        -> Tuple $ mapType f t
        Table t        -> Table $ mapType f t
        TypeApply s ts -> TypeApply s $ map (mapType f) ts
        Range t        -> Type.Range $ mapType f t
        ADT ts         -> ADT $ map (mapType f) ts
        Void           -> typ
        _ -> error (show typ)

findGenerics :: [Symbol] -> Type -> [Type]
findGenerics typeArgs typ = case typ of
    t | isSimple t -> []
    TypeApply s [] | s `elem` typeArgs -> [typ]
    TypeApply s _  | s `elem` typeArgs -> error "generic applied to arguments"
    TypeApply s ts -> concat $ map (findGenerics typeArgs) ts
    Table t -> findGenerics typeArgs t
    Void -> []
    Type _ -> []
    Record ts -> concat $ map (findGenerics typeArgs) ts
    Tuple t -> findGenerics typeArgs t
    _ -> error $ show typ


applyTypeArguments :: TypeDefs m => [Symbol] -> [Type] -> Type -> m Type
applyTypeArguments argSymbols argTypes typ = do
    when (length argSymbols /= length argTypes) $ error "invalid arguments"
    let args = zip argSymbols argTypes
    flattenTuple =<< case typ of
        TypeApply s [] -> case lookup s args of
            Just x  -> return x
            Nothing -> return typ

        TypeApply s ts -> case lookup s args of
            Just x  -> error (show x)
            Nothing -> TypeApply s <$> mapM (applyTypeArguments argSymbols argTypes) ts

        Record ts               -> Record <$> mapM (applyTypeArguments argSymbols argTypes) ts
        Tuple t                 -> Tuple <$> applyTypeArguments argSymbols argTypes t
        Table t                 -> Table <$> applyTypeArguments argSymbols argTypes t
        ADT ts                  -> ADT <$> mapM (applyTypeArguments argSymbols argTypes) ts
        _ | isSimple typ        -> return typ
        Void                    -> return typ
        _                       -> error $ "applyTypeArguments: " ++ show typ


typesCouldMatch :: TypeDefsMap -> [Symbol] -> Type -> Type -> Bool
typesCouldMatch typedefs generics t1 t2 = typesCouldMatchPure
        typedefs
        generics
        (addTuple $ runTypeDefsMonad typedefs (flattenTuple t1))
        (addTuple $ runTypeDefsMonad typedefs (flattenTuple t2))
    where
        addTuple :: Type -> Type
        addTuple typ = case typ of
            Tuple t                                   -> Tuple t
            t | (runTypeDefsMonad typedefs $ definitelyIgnoresTuples t) -> Tuple t
            t                                         -> t

        typesCouldMatchPure :: Map.Map Symbol ([Symbol], Type) -> [Symbol] -> Type -> Type -> Bool
        typesCouldMatchPure typedefs generics t1 t2 = case (t1, t2) of
            (a, b) | a == b                   -> True
            (Type _, _)                       -> True
            (_, Type _)                       -> True

            (Tuple a, Tuple b)                -> typesCouldMatchPure typedefs generics a b
            (Table a, Table b)                -> typesCouldMatch typedefs generics a b
            (Record as, Record bs)            ->
                length as == length bs &&
                all (== True) (zipWith (typesCouldMatch typedefs generics) as bs)

            -- type variables
            (TypeApply s1 ts1, TypeApply s2 ts2)
                | s1 `elem` generics && s2 `elem` generics ->
                    length ts1 == length ts2 &&
                    all (== True) (zipWith (typesCouldMatch typedefs generics) ts1 ts2)
            (x, TypeApply s ts) | s `elem` generics -> True
            (TypeApply s ts, x) | s `elem` generics -> True

            -- type defs
            (TypeApply s1 ts1, TypeApply s2 ts2) | s1 == s2 ->
                length ts1 == length ts2 &&
                all (== True) (zipWith (typesCouldMatch typedefs generics) ts1 ts2)

            _ -> False



-- Determines whether the type will change after a tuple application.
-- For example:
-- i64         -> False because ()i64 == i64
-- ()string    -> False because ()()string == ()string
-- {i64, bool} -> True  because (){i64, bool} != {i64, bool}
definitelyIgnoresTuples :: TypeDefs m => Type -> m Bool
definitelyIgnoresTuples typ = case typ of
    ADT _          -> return True
    Void           -> return True
    t | isSimple t -> return True
    Tuple t        -> return True
    Table t        -> return True
    Record ts      -> return False
    Type _         -> return False
    TypeApply s ts -> do
        resm <- Map.lookup s <$> getTypeDefs
        case resm of
            Just (ss, t) -> definitelyIgnoresTuples =<< applyTypeArguments ss ts t
            Nothing      -> return False -- must be generic

    _ -> error (show typ)

-- Recursively removes all superfluous tuple applications from the type.
-- For example:
-- ()i64         -> i64
-- ()()string    -> string
-- ()T           -> ()T
-- (){i64, bool} -> (){i64, bool}
flattenTuple :: TypeDefs m => Type -> m Type
flattenTuple typ = case typ of
    Tuple t -> do
        b <- definitelyIgnoresTuples t
        case b of
            True -> return t
            False -> return typ

    TypeApply s ts -> TypeApply s <$> mapM flattenTuple ts
    Record ts      -> Record <$> mapM flattenTuple ts
    Table t        -> Table <$> flattenTuple t
    ADT ts         -> ADT <$> mapM flattenTuple ts

    t | isSimple t -> return t
    Type _         -> return typ
    Void           -> return typ
    _ -> error (show typ)


data RecordTree
    = RecordLeaf Type Int
    | RecordTree [RecordTree]

instance Show RecordTree where
    show (RecordLeaf t i) = show t ++ "." ++ show i
    show (RecordTree rs ) = "{" ++ intercalate ", " (map show rs) ++ "}"


-- Returns the list of types represeted by a tree of records.
-- For example:
-- {i64, bool}             -> [i64, bool] 
-- { {i64, bool}, string } -> [i64, bool, string]
-- { Person, Index }       -> [string, i64, i64]   (Person == {name:string, age:i64})
-- i64                     -> [i64]
getRecordTypes :: TypeDefs m => Type -> m [Type]
getRecordTypes typ = do
    tree <- getRecordTree typ
    map fst <$> getRecordLeaves tree



getRecordTree :: TypeDefs m => Type -> m RecordTree
getRecordTree typ = do
    typeDefs <- getTypeDefs
    case typ of
        Record ts           -> RecordTree <$> applyFunc 0 ts
        TypeApply symbol ts -> case Map.lookup symbol typeDefs of
            Just (ss, Tuple t)   -> do
                applied <- applyTypeArguments ss ts (Tuple t)
                return (RecordLeaf applied 0)
            Just (ss, Record xs) -> getRecordTree =<< applyTypeArguments ss ts (Record xs)
            x -> error (show x)

        _ -> error (show typ)
    where
        applyFunc :: TypeDefs m => Int -> [Type] -> m [RecordTree]
        applyFunc offset ts = case ts of
            []     -> return []
            (x:xs) -> do
                tree <- getRecordTree' offset x
                (tree :) <$> applyFunc (getMax tree + 1) xs

        getMax :: RecordTree -> Int
        getMax tree = case tree of
            RecordLeaf _ i -> i
            RecordTree ns  -> getMax (last ns)
            _ -> error (show tree)


        getRecordTree' :: TypeDefs m => Int -> Type -> m RecordTree
        getRecordTree' offset typ = case typ of
            t | isSimple t      -> return (RecordLeaf typ offset)
            Table _             -> return (RecordLeaf typ offset)
            Tuple _             -> return (RecordLeaf typ offset)
            ADT ts              -> return (RecordLeaf typ offset)
            Record ts           -> RecordTree <$> applyFunc offset ts
            TypeApply symbol ts -> do
                resm <- Map.lookup symbol <$> getTypeDefs
                case resm of
                    Just (ss, t) -> getRecordTree' offset =<< applyTypeArguments ss ts t
                    x            -> error (show x)
            _ -> error (show typ)


getRecordLeaves :: TypeDefs m => RecordTree -> m [ (Type, Int) ]
getRecordLeaves tree = do
    case tree of
        RecordTree ns  -> concat <$> mapM getRecordLeaves ns
        RecordLeaf t i -> return [ (t, i) ]
        x              -> error (show x)


getTypeFieldIndex :: TypeDefs m => Type -> Type -> m Int
getTypeFieldIndex typ field = do
    case typ of
        Tuple (Record ts)           -> return $ fromJust (elemIndex field ts)
        Tuple (TypeApply symbol ts) -> do
            resm <- Map.lookup symbol <$> getTypeDefs
            case resm of
                Just (ss, t) -> do
                    applied <- applyTypeArguments ss ts t
                    getTypeFieldIndex applied field

        Record ts           -> return $ fromJust (elemIndex field ts)
        TypeApply symbol ts -> do
            resm <- Map.lookup symbol <$> getTypeDefs
            case resm of
                Just (ss, t) -> do
                    applied <- applyTypeArguments ss ts t
                    getTypeFieldIndex applied field
        _ -> error (show typ)

