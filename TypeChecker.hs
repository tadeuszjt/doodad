module TypeChecker where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Type as T


type NodeMap = Map.Map Key (Set.Set Node)
type Key     = String


data Node
    = Type T.Type
    | Integ
    | Agg
    | Elem Key
    | TypeOf Key
    | Conv Key
    deriving (Ord, Eq, Show)


typeCheck :: Map.Map Key S.Expr -> NodeMap
typeCheck =
    Map.foldrWithKey exprNode Map.empty 
    where
        addNode :: Key -> Node -> NodeMap -> NodeMap
        addNode name node nodeMap =
            let set = maybe Set.empty id (Map.lookup name nodeMap)
            in Map.insert name (Set.insert node set) nodeMap

        ident :: S.Expr -> Key
        ident (S.Ident _ key) = key

        exprNode :: Key -> S.Expr -> NodeMap -> NodeMap
        exprNode key expr = case expr of
            S.Cons (S.Bool _ _)   -> addNode key (Type T.Bool)
            S.Cons (S.Int _ _)    -> addNode key Integ
            S.Len _ exp           -> addNode key Integ . addNode (ident exp) Agg
            S.Append _ a b        -> addNode key Agg . addNode (ident a) Agg . addNode (ident b) Agg
            S.Infix _ S.LT a b    -> addNode key (Type T.Bool) . addNode (ident a) Integ . addNode (ident b) Integ
            S.Infix _ S.Plus a b  -> addNode key (TypeOf (ident a)) . addNode (ident a) Integ . addNode (ident b) Integ
            S.Subscript _ exp ind -> addNode key (Elem (ident exp)) . addNode (ident exp) Agg . addNode (ident ind) Integ
            S.Conv _ typ []       -> addNode key (Type typ)
            S.Call _ name []      -> addNode key (TypeOf name)
            S.Ident _ _           -> id
            _ -> error (show expr)
