module CAst where

import Data.List

data Param
    = Param { cName :: String, cType :: Type }
    deriving (Eq, Ord)

instance Show Param where
    show (Param name typ) = show typ ++ " " ++ name

data Type
    = Cint
    | Cfloat
    | Cdouble
    | Cint64_t
    | Cint32_t
    | Cint16_t
    | Cint8_t
    | Cuint64_t
    | Cuint32_t
    | Cbool
    | Cchar
    | Cstruct [Param]
    | Cunion [Param]
    | Cvoid
    | Ctypedef String
    | Cpointer Type
    deriving (Eq, Ord)

instance Show Type where
    show Cint = "int"
    show Cfloat = "float"
    show Cdouble = "double"
    show Cint8_t = "int8_t"
    show Cint16_t = "int16_t"
    show Cint32_t = "int32_t"
    show Cint64_t = "int64_t"
    show Cbool = "bool"
    show Cchar = "char"
    show (Cstruct ts) = "struct { " ++ concat (map (\t -> show t ++ "; ") ts) ++ "}"
    show (Cunion ts) = "union { " ++ concat (map (\t -> show t ++ "; ") ts) ++ "}"
    show Cvoid = "void"
    show (Ctypedef s) = s
    show (Cpointer t) = show t ++ "*"

data Operator
    = OrOr
    | Plus
    | Times
    | Minus
    | Divide
    | LTEq
    | EqEq
    deriving (Eq)

instance Show Operator where
    show OrOr = "||"
    show Plus = "+"
    show Times = "*"
    show Minus = "-"
    show Divide = "/"
    show LTEq = "<="
    show EqEq = "=="

data Expression
    = Ident String
    | Bool Bool
    | Int Integer
    | Infix Operator Expression Expression
    | String String
    | Call String [Expression]
    | CndExpr Expression Expression Expression
    | Initialiser [Expression]
    | Char Char
    | Member Expression String
    deriving (Eq)

instance Show Expression where
    show (Ident s) = s
    show (Bool b) = if b then "true" else "false"
    show (Int n) = show n
    show (String s) = show s
    show (Call name exprs) = name ++ "(" ++ intercalate ", " (map show exprs) ++ ")"
    show (CndExpr c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b
    show (Infix op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
    show (Initialiser es) = "{" ++ intercalate ", " (map show es) ++ "}"
    show (Char c) = show c
    show (Member a b) = show a ++ "." ++ b


