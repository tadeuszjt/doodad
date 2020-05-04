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
	deriving (Show, Eq)


data Pattern
	= PatIgnore TextPos
	| PatIdent TextPos String
	| PatTuple TextPos [Pattern]
	| PatArray TextPos [Pattern]
	deriving (Show, Eq)

data Expr
    = Int TextPos Integer
	| Float TextPos Double
	| Bool TextPos Bool
	| Char TextPos Char
	| String TextPos String
    | Ident TextPos String
	| Constructor TextPos Type Expr
    | Infix TextPos Op Expr Expr
	| TupleIndex TextPos Expr Int
	| Array TextPos [Expr]
	| Tuple TextPos [Expr]
	| Prefix TextPos Op Expr
	| Call TextPos String [Expr]
    deriving (Show, Eq)

data Stmt
	= Assign TextPos Pattern Expr
	| Set TextPos String Expr
	| Print TextPos [Expr]
	| Map TextPos String Expr
	| Block TextPos [Stmt]
	| Func TextPos String [Param] (Maybe Type) [Stmt]
	| Extern TextPos String [Param] (Maybe Type)
	| CallStmt TextPos String [Expr]
	| If TextPos Expr Stmt (Maybe Stmt)
	| Return TextPos (Maybe Expr)
	| Switch TextPos Expr [(Maybe Expr, Stmt)]
	deriving (Show, Eq)
