module AST where

import qualified Lexer as L

type Posn = L.AlexPosn
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
	| Print  Posn [Expr]
	deriving (Show, Eq)
