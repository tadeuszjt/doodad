module CAst where

import Prelude hiding (LT, GT)

import Data.List

data Param
    = Param { cName :: String, cType :: Type }
    | Variadic
    deriving (Eq, Ord)

instance Show Param where
    show Variadic         = "..."
    show (Param name typ) = arrStrPre typ ++ " " ++ name ++ arrStrPost typ
        where
            arrStrPost typ = case typ of
                Carray n t -> "[" ++ show n ++ "]"
                _          -> ""

            arrStrPre typ = case typ of
                Carray n t -> arrStrPre t
                _          -> show typ

data Type
    = Cint
    | Cfloat
    | Cdouble
    | Cint64_t
    | Cint32_t
    | Cint16_t
    | Cint8_t
    | Cuint8_t
    | Cuint64_t
    | Cuint32_t
    | Cbool
    | Cchar
    | Cstruct [Param]
    | Cunion [Param]
    | Cvoid
    | Ctypedef String
    | Cpointer Type
    | Carray Int Type
    deriving (Eq, Ord)

instance Show Type where
    show Cint = "int"
    show Cfloat = "float"
    show Cdouble = "double"
    show Cint8_t = "int8_t"
    show Cuint8_t = "uint8_t"
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
    show (Carray n t) = show t ++ "[" ++ show n ++ "]"

data Operator
    = OrOr
    | Plus
    | Times
    | Minus
    | Divide
    | LT
    | GT
    | LTEq
    | GTEq
    | EqEq
    | NotEq
    | Modulo
    | AndAnd
    deriving (Eq)

instance Show Operator where
    show OrOr = "||"
    show Plus = "+"
    show Times = "*"
    show Minus = "-"
    show Divide = "/"
    show LT = "<"
    show GT = ">"
    show LTEq = "<="
    show GTEq = ">="
    show EqEq = "=="
    show NotEq = "!="
    show Modulo = "%"
    show AndAnd = "&&"

data Expression
    = Ident String
    | Bool Bool
    | Int Integer
    | Float Double
    | Infix Operator Expression Expression
    | String String
    | Char Char
    | Call String [Expression]
    | CndExpr Expression Expression Expression
    | Initialiser [Expression]
    | Member Expression String
    | PMember Expression String
    | Increment Expression
    | Subscript Expression Expression
    | Deref Expression
    | Address Expression
    | Not Expression
    | Sizeof Expression
    | Cast Type Expression
    deriving (Eq)

instance Show Expression where
    show (Ident s) = s
    show (Bool b) = if b then "true" else "false"
    show (Int n) = show n
    show (Float f) = show f
    show (String s) = show s
    show (Call name exprs) = name ++ "(" ++ intercalate ", " (map show exprs) ++ ")"
    show (CndExpr c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b
    show (Infix op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
    show (Initialiser es) = "{" ++ intercalate ", " (map show es) ++ "}"
    show (Member a b) = show a ++ "." ++ b
    show (PMember a b) = show a ++ "->" ++ b
    show (Increment e) = show e ++ "++"
    show (Subscript e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"
    show (Deref e) = "(*" ++ show e ++ ")"
    show (Address e) = "&(" ++ show e ++ ")"
    show (Not e) = "!" ++ show e
    show (Char c) = show c
    show (Sizeof e) = "sizeof(" ++ show e ++ ")"
    show (Cast t e) = "(" ++ show t ++ ")(" ++ show e ++ ")"

data ID =
    ID Int
    deriving (Show, Eq, Ord)

data Element
    = Global { globalBody :: [ID] }
    | Embed String
    | For
        { forInit :: Maybe Expression
        , forCnd :: Maybe Expression
        , forPost :: Maybe Expression
        , forBody :: [ID]
        }
    | Extern
        { extName :: String
        , extRetty :: Type
        , extArgs :: [Type]
        }
    | Func
        { funcBody :: [ID]
        , funcName :: String
        , funcArgs :: [Param]
        , funcRetty :: Type
        }
    | Typedef
        { typedefName :: String
        , typedefType :: Type
        }
    | Return Expression
    | ReturnVoid
    | Break
    | Assign Type String Expression
    | Set Expression Expression
    | Goto String
    | Label String
    | If
        { ifExpr :: Expression
        , ifStmts :: [ID]
        }
    | Else
        { elseStmts :: [ID]
        }
    | ExprStmt Expression
    | Switch
        { switchExpr :: Expression
        , switchBody :: [ID]
        }
    | Case
        { caseExpr :: Expression
        , caseBody :: [ID]
        }
    deriving (Show)
 