module Type where

import Control.Monad
import qualified Data.Map as Map
import Data.List
import Symbol

type TypeDefsMap = Map.Map Symbol ([Symbol], Type)

class Monad m => TypeDefs m where getTypeDefs :: m TypeDefsMap

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
    | Sum
    | Func
    | Slice
    | Tuple
    | Table
    | Array
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
        Array         -> "Array"
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


mapType :: (Type -> Type) -> Type -> Type
mapType f typ = f $ case typ of
    Apply t ts -> Apply (mapType f t) $ map (mapType f) ts
    _          -> typ


applyType :: [(Type, Type)] -> Type -> Type
applyType subs = mapType (f subs)
    where
        f :: [(Type, Type)] -> Type -> Type
        f []          z = z
        f ((x, u):xs) z = f xs (if z == x then u else z)


applyTypeM :: MonadFail m => [Symbol] -> [Type] -> Type -> m Type
applyTypeM argSymbols argTypes typ = do
    unless (length argSymbols == length argTypes) (fail $ "invalid arguments: " ++ show typ)
    let subs = zip (map TypeDef argSymbols) argTypes
    return (applyType subs typ)


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
        baseTypeOfm =<< applyTypeM ss ts t

    TypeDef s -> do
        Just ([], t) <- Map.lookup s <$> getTypeDefs
        baseTypeOfm t

    _ -> return $ Just (typeof a)


typesCouldMatch :: Type -> Type -> Bool
typesCouldMatch t1 t2 = case (t1, t2) of
    (a, b) | a == b            -> True
    (Type _, _)                -> True
    (_, Type _)                -> True

    (Apply t1 ts1, Apply t2 ts2)
        | length ts1 == length ts2 ->
            all id $ zipWith typesCouldMatch (t1 : ts1) (t2 : ts2)

    (TypeDef s1, TypeDef s2) -> symbolsCouldMatch s1 s2

    _ -> False


typeFullyResolved :: Type -> Bool
typeFullyResolved typ = case typ of
    Type _         -> False
    Apply t ts     -> all typeFullyResolved (t : ts)
    _              -> True
