module AST where

import qualified Cmp
import qualified Lexer    as L

type Posn = Cmp.TextPos
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
		{ paramPos  :: Posn
		, paramName :: String
		, paramType :: Type
		}
	deriving (Show, Eq)


data Type
	= I64
	| TBool
	| TTuple [Type]
	deriving (Show, Eq)


data Pattern
	= PatIgnore Posn
	| PatIdent Posn String
	| PatTuple Posn [Pattern]
	deriving (Show, Eq)

data Expr
    = Int   Posn Integer
	| Bool  Posn Bool
    | Ident Posn String
    | Infix Posn Op Expr Expr
	| TupleIndex Posn Expr Int
	| Array Posn [Expr]
	| Tuple Posn [Expr]
	| Prefix Posn Op Expr
	| Call  Posn String [Expr]
	| String Posn String
    deriving (Show, Eq)


data Stmt
	= Assign Posn Pattern Expr
	| Set Posn String Expr
	| Print  Posn [Expr]
	| Map Posn String Expr
	| Block Posn [Stmt]
	| Func Posn String [Param] (Maybe Type) [Stmt]
	| CallStmt Posn String [Expr]
	| If Posn Expr Stmt (Maybe Stmt)
	| Return Posn (Maybe Expr)
	| Switch Posn Expr [(Maybe Expr, Stmt)]
	deriving (Show, Eq)
