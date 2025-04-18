{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import Symbol
import Data.List

type TypeDefsMap = Map.Map Symbol ([Symbol], Type)

class MonadFail m => MonadTypeDefs m where getTypeDefs :: m TypeDefsMap

class Typeof a where typeof :: a -> Type
instance Typeof Type where typeof = id


newtype TypeDefsMonad a = TypeDefsMonad
    { unTypeDefsMonad :: ReaderT TypeDefsMap (Except String) a }
    deriving (Functor, Applicative, Monad, MonadReader TypeDefsMap, MonadError String)


instance MonadFail TypeDefsMonad where
    fail = throwError


instance MonadTypeDefs TypeDefsMonad where
    getTypeDefs = ask


runTypeDefsMonad :: TypeDefsMap -> TypeDefsMonad a -> Either String a
runTypeDefsMonad typeDefs f =
    runExcept $ runReaderT (unTypeDefsMonad f) typeDefs


data Type
    = Type Int
    | U8
    | I8                     
    | I16                    
    | I32                    
    | I64                    
    | F32                    
    | F64                    
    | Bool                   
    | Char                   
    | Sum
    | Ref
    | Func
    | Slice
    | Tuple
    | Table
    | Array
    | Size Int
    | TypeDef Symbol
    | Apply Type Type
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        Type id       -> "t" ++ show id
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
        Ref           -> "Ref"
        Slice         -> "Slice"
        Func          -> "Func"
        Array         -> "Array"
        TypeDef s     -> prettySymbol s
        Apply t1 t2   -> let (x, xs) = unfoldType t in
            show x ++ "{" ++ intercalate ", " (map show xs) ++ "}"

        Size n        -> show n
        x -> error ""


-- generates an alphanum code for use in function names
typeCode :: Type -> String
typeCode typ = case unfoldType typ of
    (TypeDef s, xs) -> sym s ++ concat (map typeCode xs)
    (x, xs)         -> show x ++ concat (map typeCode xs)

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

foldType :: [Type] -> Type
foldType (x:xs) = foldl Apply x xs


unfoldType :: Type -> (Type, [Type])
unfoldType typ = case typ of
    Apply t1 t2 -> let (x, xs) = unfoldType t1 in (x, xs ++ [t2])
    _         -> (typ, [])


mapType :: (Type -> Type) -> Type -> Type
mapType f typ = f $ case typ of
    Apply t1 t2 -> Apply (mapType f t1) (mapType f t2)
    _           -> typ


mapTypeM :: Monad m => (Type -> m Type) -> Type -> m Type
mapTypeM f typ = do
    f =<< case typ of
        Apply t1 t2     -> do
            t1' <- mapTypeM f t1
            t2' <- mapTypeM f t2
            return (Apply t1' t2')
        _ -> return typ


applyType :: [(Type, Type)] -> Type -> Type
applyType subs = mapType (fun subs)
    where
        fun :: [(Type, Type)] -> Type -> Type
        fun []          z = z
        fun ((x, u):xs) z = fun xs (if z == x then u else z)


applyTypeM :: MonadFail m => [Symbol] -> [Type] -> Type -> m Type
applyTypeM argSymbols argTypes typ = do
    let subs = zip (map TypeDef argSymbols) argTypes
    let (x, xs) = unfoldType (applyType subs typ)
    unless (length argSymbols <= length argTypes) (fail "invalid type arguments")
    return $ foldType $ (x : xs) ++ drop (length argSymbols) argTypes


baseTypeOf :: (MonadTypeDefs m, Typeof a) => a -> m Type
baseTypeOf a = do
    resm <- baseTypeOfm a
    case resm of
        Nothing -> fail ("baseTypeOfm returned Nothing: " ++ show (typeof a))
        Just x  -> return x


baseTypeOfm :: (MonadTypeDefs m, Typeof a) => a -> m (Maybe Type)
baseTypeOfm a = case unfoldType (typeof a) of
    (Type _, _) -> return Nothing

    (TypeDef s, ts) -> do
        resm <- Map.lookup s <$> getTypeDefs
        (ss, t) <- case resm of -- [R], Func{R, Io}
            Nothing -> error $ prettySymbol s ++ " isn't in typeDefs"
            Just x -> return x
        baseTypeOfm =<< applyTypeM ss ts t

    (_, _) -> return $ Just (typeof a)


lowerTypeOfm :: (MonadTypeDefs m, Typeof a) => a -> m (Maybe Type) 
lowerTypeOfm a = case unfoldType (typeof a) of
    (Type _, _) -> return Nothing
    (TypeDef s, ts) -> do
        Just (ss, t) <- Map.lookup s <$> getTypeDefs
        Just <$> applyTypeM ss ts t

    _ -> error "type isn't a TypeDef"


typesCouldMatch :: Type -> Type -> Bool
typesCouldMatch t1 t2 = case (t1, t2) of
    (a, b) | a == b            -> True
    (Type _, _)                -> True
    (_, Type _)                -> True
    (Apply a1 a2, Apply b1 b2) -> typesCouldMatch a1 b1 && typesCouldMatch a2 b2
    (TypeDef s1, TypeDef s2)   -> symbolsCouldMatch s1 s2

    _ -> False


typeFullyResolved :: Type -> Bool
typeFullyResolved typ = case typ of
    Type _         -> False
    Apply t1 t2    -> all typeFullyResolved [t1, t2]
    _              -> True


-- function to determine if a type could not be overridden by another type when describing a type.
typeFullyDescribes :: Type -> Type -> Bool
typeFullyDescribes t1 t2 = case (t1, t2) of
    (Apply a1 b1, Apply a2 b2) -> typeFullyDescribes a1 a2 && typeFullyDescribes b1 b2
    (Apply _ _, _) -> False

    (x, y) | isSimple x -> x == y
    (Table, b) -> b == Table
    (Tuple, b) -> b == Tuple
    (Slice, b) -> b == Slice
    (Sum, b)   -> b == Sum
    (Type.Array, b) -> b == Type.Array
    (TypeDef s1, b) -> b == TypeDef s1

    (Type _, _) -> True
    (_, Type _) -> False

    (Size a, Size b) -> a == b

    x -> error (show x)


