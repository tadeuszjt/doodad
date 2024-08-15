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
    | Csize_t
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
    show Csize_t  = "size_t"
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

data Qualifier
    = Static
    | Const
    | Extern
    | Inline
    deriving (Eq)

instance Show Qualifier where
    show Static = "static"
    show Const = "const"
    show Extern = "extern"
    show Inline = "inline"

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
    | Prefix Operator Expression
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
    | SizeofType Type
    | Cast Type Expression
    deriving (Eq)

instance Show Expression where
    show (Ident s) = s
    show (Bool b) = if b then "true" else "false"
    show (Int n) = show n
    show (Float f) = show f
    show (String s) = show s
    show (Call name exprs) = name ++ "(" ++ intercalate ", " (map showNoParens exprs) ++ ")"
    show (CndExpr c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b
    show (Infix op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
    show (Prefix op a) = "(" ++ show op ++ " " ++ show a ++ ")"
    show (Initialiser es) = "{" ++ intercalate ", " (map showNoParens es) ++ "}"
    show (Member a b) = show a ++ "." ++ b
    show (PMember a b) = show a ++ "->" ++ b
    show (Increment e) = show e ++ "++"
    show (Subscript e1 e2) = show e1 ++ "[" ++ showNoParens e2 ++ "]"
    show (Deref e) = "(*" ++ show e ++ ")"
    show (Address (Ident s)) = "(&" ++ s ++ ")"
    show (Address e) = "&(" ++ showNoParens e ++ ")"
    show (Not e) = "!" ++ show e
    show (Char '\0') = "'\\0'"
    show (Char c) = show c
    show (Sizeof e) = "sizeof(" ++ showNoParens e ++ ")"
    show (SizeofType t) = "sizeof(" ++ show t ++ ")"
    show (Cast t e) = "(" ++ show t ++ ")(" ++ showNoParens e ++ ")"

showNoParens :: Expression -> String
showNoParens expr@(Cast _ _) = show expr
showNoParens expr = let str = show expr in
    if head str == '(' && last str == ')' then init (tail str) else str

data ID =
    ID Int
    deriving (Show, Eq, Ord)

data Element
    = Global { globalBody :: [ID] }
    | Embed String
    | Return Expression
    | ReturnVoid
    | Break
    | Assign Type String Expression
    | Set Expression Expression
    | Goto String
    | Label String
    | ExprStmt Expression
    | For
        { forInit :: Maybe Expression
        , forCnd :: Maybe Expression
        , forPost :: Maybe Expression
        , forBody :: [ID]
        }
    | ExternFunc
        { extName :: String
        , extRetty :: Type
        , extArgs :: [Type]
        , extQualifiers :: [Qualifier]
        }
    | Func
        { funcBody :: [ID]
        , funcName :: String
        , funcArgs :: [Param]
        , funcRetty :: Type
        , funcQualifiers :: [Qualifier]
        }
    | Typedef
        { typedefName :: String
        , typedefType :: Type
        }
    | If
        { ifExpr :: Expression
        , ifStmts :: [ID]
        }
    | Else
        { elseStmts :: [ID]
        }
    | Switch
        { switchExpr :: Expression
        , switchBody :: [ID]
        }
    | Case
        { caseExpr :: Expression
        , caseBody :: [ID]
        }
    deriving (Show, Eq)
 
