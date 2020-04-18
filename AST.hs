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


data Expr
    = Int   Posn Int
    | Ident Posn String 
    | Infix Posn Op Expr Expr
    deriving (Show, Eq)


data Stmt
	= Assign Posn String Expr
	| Set Posn String Expr
	| Print  Posn [Expr]
	| Block Posn [Stmt]
	| Func Posn String Stmt
	| Call Posn String [Expr]
	deriving (Show, Eq)
