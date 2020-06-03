module AST where

import           Data.Word
import           Type
import           Error
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
    = PatLiteral Constant
    | PatIgnore  TextPos
    | PatIdent   TextPos String
    | PatTuple   TextPos [Pattern]
    | PatArray   TextPos [Pattern]
    | PatTyped   TextPos String Pattern
    deriving (Show, Eq)


data Index
    = IndIdent TextPos String
    | IndArray TextPos Index Expr
    | IndTuple TextPos Index Word32
    deriving (Show, Eq)


data Constant
    = Int TextPos Integer
    | Float TextPos Double
    | Bool TextPos Bool
    | Char TextPos Char
    | String TextPos String
    deriving (Show, Eq)


data Expr
    = Cons Constant
    | Tuple TextPos [Expr]
    | Array TextPos [Expr]
    | Table TextPos [[Expr]]
    | Member TextPos Expr String
    | Subscript TextPos Expr Expr
    | TupleIndex TextPos Expr Word32
    | Ident TextPos String
    | Call TextPos String [Expr]
    | Conv TextPos Type [Expr]
    | Len TextPos Expr
    | Append TextPos Expr Expr
    | Prefix TextPos Op Expr
    | Infix TextPos Op Expr Expr
    deriving (Show, Eq)


data Stmt
    = Assign TextPos Pattern Expr
    | Set TextPos Index Expr
    | Print TextPos [Expr]
    | Block TextPos [Stmt]
    | Func TextPos String [Param] (Maybe Type) [Stmt]
    | Extern TextPos String [Param] (Maybe Type)
    | Typedef TextPos String Type
    | Datadef TextPos String [Data] 
    | CallStmt TextPos String [Expr]
    | If TextPos Expr Stmt (Maybe Stmt)
    | Return TextPos (Maybe Expr)
    | Switch TextPos Expr [(Pattern, Stmt)]
    | While  TextPos Expr [Stmt]
    deriving (Show, Eq)
