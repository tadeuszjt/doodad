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
	= TBool
	| TI32
	| TI64
	| TChar
	| TString
	| TArray Int Type
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
	| Char Posn Char
	| String Posn String
    | Ident Posn String
	| Constructor Posn Type Expr
    | Infix Posn Op Expr Expr
	| TupleIndex Posn Expr Int
	| Array Posn [Expr]
	| Tuple Posn [Expr]
	| Prefix Posn Op Expr
	| Call  Posn String [Expr]
    deriving (Show, Eq)


data Stmt
	= Assign Posn Pattern Expr
	| Set Posn String Expr
	| Print  Posn [Expr]
	| Map Posn String Expr
	| Block Posn [Stmt]
	| Func Posn String [Param] (Maybe Type) [Stmt]
	| Extern Posn String [Param] (Maybe Type)
	| CallStmt Posn String [Expr]
	| If Posn Expr Stmt (Maybe Stmt)
	| Return Posn (Maybe Expr)
	| Switch Posn Expr [(Maybe Expr, Stmt)]
	deriving (Show, Eq)
