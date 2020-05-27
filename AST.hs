module AST where

import           Data.Word
import           CmpMonad (TextPos)
import           Type
import qualified Lexer    as L


type AST  = [Stmt]


data Op
    = Plus
    | Minus
    | Times
    | Divide
    | Mod
    | LT
    | GT
    | LTEq
    | GTEq
    | EqEq
    | OrOr
    | AndAnd
    deriving (Show, Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: String
        , paramType :: Type
        }
    deriving (Show, Eq)


data Data
    = DataIdent { dataPos :: TextPos, dataSymbol :: String }
    | DataFunc  { dataPos :: TextPos, dataSymbol :: String, dataParams :: [Param] }
    deriving (Show, Eq)


data Pattern
    = PatIgnore  { patPos :: TextPos }
    | PatLiteral { lit :: Expr }
    | PatIdent   { patPos :: TextPos, symbol :: String }
    | PatTuple   { patPos :: TextPos, patterns :: [Pattern] }
    | PatArray   { patPos :: TextPos, patterns :: [Pattern] }
    | PatTyped   { patPos :: TextPos, symbol :: String, pattern :: Pattern }
    deriving (Show, Eq)


data Index
    = IndIdent { indPos :: TextPos, indSym :: String }
    | IndArray { indPos :: TextPos, index :: Index, expr :: Expr }
    | IndTuple { indPos :: TextPos, index :: Index, tupleIdx :: Word32 }
    deriving (Show, Eq)


data Expr
    = Int TextPos Integer
    | Float TextPos Double
    | Bool TextPos Bool
    | Char TextPos Char
    | String TextPos String
    | Tuple TextPos [Expr]
    | Array TextPos [Expr]
    | Table TextPos [[Expr]]
    | TupleIndex TextPos Expr Word32
    | TupleMember TextPos Expr String
    | ArrayIndex TextPos Expr Expr
    | Ident TextPos String
    | Call TextPos String [Expr]
    | Conv TextPos Type [Expr]
    | Len TextPos Expr
    | Prefix TextPos Op Expr
    | Infix TextPos Op Expr Expr
    deriving (Show, Eq)


data Stmt
    = Assign TextPos Pattern Expr
    | Set TextPos Index Expr
    | Print TextPos [Expr]
    | Block TextPos [Stmt]
    | Func TextPos String [Pattern] (Maybe Type) [Stmt]
    | Extern TextPos String [Pattern] (Maybe Type)
    | Typedef TextPos String Type
    | Datadef TextPos String [Data] 
    | CallStmt TextPos String [Expr]
    | If TextPos Expr Stmt (Maybe Stmt)
    | Return TextPos (Maybe Expr)
    | Switch TextPos Expr [(Pattern, Stmt)]
    | While  TextPos Expr [Stmt]
    deriving (Show, Eq)
