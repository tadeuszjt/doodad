module AST where

import           Cmp      (TextPos)
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

data Type
	= TBool
	| TI32
	| TI64
	| TF32
	| TF64
	| TChar
	| TString
	| TArray Int Type
	| TTuple [Type]
    | TIdent String
	deriving (Show, Eq)


data Pattern
	= PatIgnore { pos :: TextPos }
	| PatIdent  { pos :: TextPos, symbol :: String }
	| PatTuple  { pos :: TextPos, patterns :: [Pattern] }
	| PatArray  { pos :: TextPos, patterns :: [Pattern] }
	deriving (Show, Eq)

data Expr
    = Int TextPos Integer
	| Float TextPos Double
	| Bool TextPos Bool
	| Char TextPos Char
	| String TextPos String
	| Tuple TextPos [Expr]
	| Array TextPos [Expr]
	| TupleIndex TextPos Expr Int
    | ArrayIndex TextPos Expr Expr
    | Ident TextPos String
	| Call TextPos String [Expr]
	| Constructor TextPos Type Expr
    | Len TextPos Expr
	| Prefix TextPos Op Expr
    | Infix TextPos Op Expr Expr
    deriving (Show, Eq)

data Stmt
	= Assign TextPos Pattern Expr
	| Set TextPos String Expr
	| Print TextPos [Expr]
	| Map TextPos String Expr
	| Block TextPos [Stmt]
	| Func TextPos String [Param] (Maybe Type) [Stmt]
	| Extern TextPos String [Param] (Maybe Type)
    | Typedef TextPos String Type
	| CallStmt TextPos String [Expr]
	| If TextPos Expr Stmt (Maybe Stmt)
	| Return TextPos (Maybe Expr)
	| Switch TextPos Expr [(Maybe Expr, Stmt)]
	deriving (Show, Eq)
