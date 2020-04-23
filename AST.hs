	module AST where

import qualified Lexer as L
import qualified CmpState

type Posn = CmpState.TextPos
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
	deriving (Show, Eq)


data Expr
    = Int   Posn Int
	| Bool  Posn Bool
    | Ident Posn String 
    | Infix Posn Op Expr Expr
	| Call  Posn String [Expr]
    deriving (Show, Eq)


data Stmt
	= Assign Posn String Expr
	| Set Posn String Expr
	| Print  Posn [Expr]
	| Block Posn [Stmt]
	| Func Posn String [Param] (Maybe Type) Stmt
	| CallStmt Posn String [Expr]
	| If Posn Expr Stmt (Maybe Stmt)
	| Return Posn (Maybe Expr)
	deriving (Show, Eq)
