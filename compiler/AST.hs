module AST where

import Prelude hiding (LT, GT)
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Type (Type, Type(Void), Typeof, typeof)
import Error
import Symbol

type ModuleName = String

data AST
    = AST
        { astModuleName :: ModuleName
        , astImports    :: [Import]
        , astStmts      :: [Stmt]
        }
    deriving (Eq)


data Operator
    = Plus
    | Minus
    | Times
    | Divide
    | Modulo
    | LT
    | GT
    | Eq
    | LTEq
    | GTEq
    | EqEq
    | OrOr
    | AndAnd
    | NotEq
    | Not
    | PlusEq
    deriving (Eq, Ord)


data Import
    = Import FilePath
    | CInclude String
    | CLink String
    deriving (Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: Symbol
        , paramType :: Type
        }
    deriving (Eq, Ord)

instance Type.Typeof Param where
    typeof (Param pos name typ) = typ


data Pattern
    = PatLiteral   Expr
    | PatIgnore    TextPos
    | PatIdent     TextPos Symbol
    | PatTuple     TextPos [Pattern]
    | PatArray     TextPos [Pattern]
    | PatGuarded   TextPos Pattern Expr
    | PatField     TextPos Symbol [Pattern]
    | PatTypeField TextPos Type Pattern
    | PatAnnotated Pattern Type
    | PatRecord    TextPos [Pattern]
    | PatNull      TextPos
    deriving (Eq)

data Expr
    = AExpr       Type  Expr
    | Int         TextPos Integer
    | Float       TextPos Double
    | Bool        TextPos Bool
    | Char        TextPos Char
    | String      TextPos String
    | Tuple       TextPos [Expr]
    | Call        TextPos [Expr] Symbol [Expr]
    | Construct   TextPos Symbol [Expr]
    | Null        TextPos 
    | Field       TextPos Expr Symbol
    | Subscript   TextPos Expr Expr
    | Ident       TextPos Symbol
    | Builtin     TextPos [Expr] String [Expr]
    | Prefix      TextPos Operator Expr
    | Infix       TextPos Operator Expr Expr
    | Match       TextPos Expr Pattern
    | Range       TextPos (Maybe Expr) (Maybe Expr) (Maybe Expr)
    | RecordAccess TextPos Expr
    | Array       TextPos [Expr]
    deriving (Eq)

instance Typeof Expr where
    typeof (AExpr t e) = t
    typeof a = error $ "can only take typeof AExpr: " ++ show a

data Stmt
    = Assign      TextPos Pattern Expr
    | SetOp       TextPos Operator Expr   Expr
    | ExprStmt    Expr
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Expr Stmt (Maybe Stmt)
    | While       TextPos Expr Stmt
    | FuncDef     TextPos [Symbol] [Param] Symbol [Param] Type Stmt
    | Typedef     TextPos [Symbol] Symbol AnnoType
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Expr (Maybe Pattern) Stmt
    | Data        TextPos Symbol Type (Maybe Expr)
    | EmbedC      TextPos String
    | Const       TextPos Symbol Expr
    | Increment   TextPos Expr
    deriving (Eq, Show)


data AnnoType
    = AnnoType  Type
    | AnnoTuple [Param]
    | AnnoTable [Param]
    | AnnoADT   [Param]
    deriving (Eq)


instance TextPosition Pattern where
    textPos pattern = case pattern of
        PatLiteral   e -> textPos e
        PatIgnore    p -> p
        PatIdent     p _ -> p
        PatTuple     p _ -> p
        PatArray     p _ -> p
        PatGuarded   p _ _ -> p
        PatField     p _ _ -> p
        PatTypeField p _ _ -> p
        PatAnnotated pat _ -> textPos pat
        PatNull      p -> p


instance TextPosition Expr where
    textPos expr = case expr of
        AExpr        t e -> textPos e
        Int          p _ -> p
        Float        p _ -> p
        Bool         p _ -> p
        Char         p _ -> p
        Null         p -> p
        String       p _ -> p
        Tuple        p _ -> p
        Field        p _ _ -> p
        Subscript    p _ _ -> p
        Ident        p _ -> p
        Call         p _ _ _ -> p 
        Builtin      p _ _ _ -> p 
        Prefix       p _ _ -> p
        Infix        p _ _ _ -> p
        Match        p _ _ -> p
        Range        p _ _ _ -> p
        Array        p _ -> p
        Construct    p _ _ -> p
        RecordAccess p _ -> p
        _ -> error (show expr)


instance TextPosition Stmt where
    textPos stmt = case stmt of
        Assign      p _ _ -> p
        ExprStmt    e -> textPos e
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncDef     p _ _ _ _ _ _ -> p
        Typedef     p _ _ _ -> p
        Switch      p _ _ -> p
        For         p _ _ _ -> p
        Data        p _ _ _ -> p
        EmbedC      p _ -> p
        SetOp       p _ _ _ -> p
        Const       p _ _ -> p
        _ -> error (show stmt)

tupStrs, arrStrs, brcStrs :: [String] -> String
tupStrs strs = "(" ++ intercalate ", " strs ++ ")"
arrStrs strs = "[" ++ intercalate ", " strs ++ "]"
brcStrs strs = "{" ++ intercalate ", " strs ++ "}"


instance Show Import where
    show (Import path) = "import " ++ path
    show (CInclude path) = "#include " ++ path
    show (CLink path) = "link " ++ path

instance Show Param where
    show (Param pos name Void) = show name
    show (Param pos name typ)  = show name ++ ":" ++ show typ


instance Show Operator where
    show op = case op of
        Plus   -> "+"
        Minus  -> "-"
        Times  -> "*"
        Divide -> "/"
        Modulo -> "%"
        LT     -> "<"
        GT     -> ">"
        Eq     -> "="
        LTEq   -> "<="
        GTEq   -> ">="
        EqEq   -> "=="
        OrOr   -> "||"
        AndAnd -> "&&"
        NotEq  -> "!="
        Not    -> "!"
        PlusEq -> "+="

instance Show AnnoType where
    show annoType = case annoType of
        AnnoType t   -> show t
        AnnoTuple ps -> tupStrs $ map show ps
        AnnoADT ps   -> brcStrs $ map show ps
        AnnoTable xs -> "[" ++ intercalate "; " (map show xs) ++ "]"


instance Show Pattern where
    show pat = case pat of
        PatLiteral c             -> show c
        PatIgnore pos            -> "_"
        PatIdent pos symbol      -> show symbol
        PatTuple pos ps          -> tupStrs (map show ps)
        PatArray pos ps          -> arrStrs (map show ps)
        PatGuarded pos pat expr  -> show pat ++ " | " ++ show expr
        PatField pos symbol pats -> show symbol ++ tupStrs (map show pats)
        PatTypeField pos typ pat -> show pat ++ arrStrs [show typ]
        PatAnnotated pat typ     -> show pat ++ ":" ++ show typ
        PatRecord pos pats       -> brcStrs (map show pats)
        PatNull pos              -> "null"


instance Show Expr where
    show expr = case expr of
        AExpr t e                     -> show e ++ ":" ++ show t 
        Int pos n                     -> show n
        Float pos f                   -> show f
        Bool pos b                    -> if b then "true" else "false"
        Char pos c                    -> show c
        Null p                        -> "null"
        String pos s                  -> show s
        Tuple pos exprs               -> tupStrs (map show exprs)
        Field pos expr symbol         -> show expr ++ "." ++ show symbol
        Subscript pos expr1 expr2     -> show expr1 ++ "[" ++ show expr2 ++ "]"
        Ident p s                     -> show s 
        Prefix pos op expr            -> show op ++ show expr
        Infix pos op expr1 expr2      -> show expr1 ++ " " ++ show op ++ " " ++ show expr2
        Call pos [] symbol exprs      -> show symbol ++ tupStrs (map show exprs)
        Call pos [param] symbol exprs -> show param ++ "." ++ show symbol ++ tupStrs (map show exprs)
        Call pos params symbol exprs  -> brcStrs (map show params) ++ "." ++ show symbol ++ tupStrs (map show exprs)
        Builtin pos [] sym exprs      -> sym ++ tupStrs (map show exprs)
        Builtin pos params sym exprs  -> brcStrs (map show params) ++ "." ++ sym ++ tupStrs (map show exprs)
        Match pos expr1 expr2         -> "(" ++ show expr1 ++ " -> " ++ show expr2 ++ ")"
        Range pos mexpr mexpr1 mexpr2 -> maybe "" show mexpr ++ "[" ++ maybe "" show mexpr1 ++ ".." ++ maybe "" show mexpr2 ++ "]"
        Array pos exprs               -> arrStrs (map show exprs)
        Construct pos symbol exprs    -> show symbol ++ tupStrs (map show exprs)
        RecordAccess pos expr         -> show expr ++ "{}"


-- every function must end on a newline and print pre before every line
prettyAST :: AST -> IO ()
prettyAST ast = do
    putStrLn $ "module " ++ (astModuleName ast)

    putStrLn ""

    forM_ (astImports ast) $ \imp ->
        putStrLn $ show imp

    putStrLn ""

    mapM_ (prettyStmt "") (astStmts ast)


prettyStmt :: String -> Stmt -> IO ()
prettyStmt pre stmt = case stmt of
    Increment pos expr -> putStrLn $ pre ++ show expr ++ "++"
    FuncDef pos typeArgs params symbol args retty blk -> do
        typeArgsStr <- case typeArgs of
            [] -> return ""
            as -> return $ arrStrs $ map show as
        paramStr <- case params of
            [] -> return ""
            ps -> return $ brcStrs $ map show ps
        putStrLn $ pre
            ++ "fn"
            ++ typeArgsStr ++ " "
            ++ paramStr
            ++ show symbol
            ++ tupStrs (map show args)
            ++ " "
            ++ if retty == Void then "" else show retty
        prettyStmt (pre ++ "\t") blk
        putStrLn ""

    Assign pos pat expr    -> putStrLn $ pre ++ "let " ++ show pat ++ " = " ++ show expr
    SetOp _ op expr1 expr2 -> putStrLn $ pre ++ (show expr1) ++ " " ++ show op ++ " " ++ show expr2
    Return pos mexpr       -> putStrLn $ pre ++ "return " ++ maybe "" show mexpr
    Const _ symbol expr    -> putStrLn $ pre ++ "const " ++ show symbol ++ " = " ++ show expr

    If pos cnd true mfalse -> do
        putStrLn $ pre ++ "if " ++ show cnd
        prettyStmt (pre ++ "\t") true
        putStrLn $ pre ++ "else"
        maybe (return ()) (prettyStmt (pre ++ "\t")) mfalse

    ExprStmt callExpr -> putStrLn $ pre ++ show callExpr
            
    Block stmts -> do
        mapM_ (prettyStmt pre) stmts

    While pos cnd stmt -> do
        putStrLn $ pre ++ "while " ++ show cnd
        prettyStmt (pre ++ "\t") stmt

    Typedef pos typeArgs symbol anno -> do
        argStr <- case typeArgs of
            [] -> return ""
            xs -> return $ "[" ++ intercalate ", " (map show xs) ++ "]"
        putStrLn $ pre ++ "type" ++ argStr ++ " " ++ show symbol ++ " " ++ show anno

    Switch pos expr cases -> do
        putStrLn $ pre ++ "switch " ++ show expr
        forM_ cases $ \(pat, stmt) -> do
            putStrLn $ pre ++ "\t" ++ show pat
            prettyStmt (pre ++ "\t\t") stmt

    For pos expr mcnd blk -> do
        let cndStr = maybe "" ((" -> " ++) . show) mcnd
        let exprStr = "" ++ show expr
        putStrLn $ pre ++ "for " ++ exprStr ++ cndStr
        prettyStmt (pre ++ "\t") blk

    Data pos symbol typ mexpr -> do
        putStrLn $ pre ++ "data " ++ show symbol ++ " " ++ show typ ++ maybe "" ((" " ++) . show) mexpr

    EmbedC pos str -> do
        putStrLn $ pre ++ "$" ++ str

    _  -> error $ "invalid stmt: " ++ show stmt

