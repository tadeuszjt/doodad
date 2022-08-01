module AST where

import Prelude hiding (LT, GT)
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Type (Type, Type(Void))
import Error
import Symbol

type ModuleName = String

showPath path = concat (intersperse "/" path)

data AST
    = AST
        { astModuleName :: Maybe ModuleName
        , astImports    :: [Import]
        , astStmts      :: [Stmt]
        }
    deriving (Eq)


data Op
    = Plus
    | Minus
    | Times
    | Divide
    | Modulo
    | LT
    | GT
    | LTEq
    | GTEq
    | EqEq
    | OrOr
    | AndAnd
    | NotEq
    | Not
    deriving (Eq, Ord)


data Import
    = Import FilePath
    | ImportC FilePath
    | ImportCMacro String Type
    deriving (Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: Symbol
        , paramType :: Type
        }
    deriving (Eq)


data Append
    = AppendTable TextPos Append Expr
    | AppendIndex Index
    deriving (Eq)

data Pattern
    = PatLiteral   Expr
    | PatIgnore    TextPos
    | PatIdent     TextPos Symbol
    | PatTuple     TextPos [Pattern]
    | PatArray     TextPos [Pattern]
    | PatGuarded   TextPos Pattern Expr
    | PatField     TextPos Symbol [Pattern]
    | PatAnnotated Pattern Type
    deriving (Eq)

data Index
    = IndIdent TextPos Symbol
    | IndArray TextPos Index Expr
    | IndTuple TextPos Index Word32
    deriving (Eq)

data Condition
    = CondExpr Expr
    | CondMatch Pattern Expr
    deriving (Eq)

data Expr
    = AExpr      Type  Expr
    | Int        TextPos Integer
    | Float      TextPos Double
    | Bool       TextPos Bool
    | Char       TextPos Char
    | String     TextPos String
    | Tuple      TextPos [Expr]
    | Table      TextPos [[Expr]]
    | Member     TextPos Expr String
    | Subscript  TextPos Expr Expr
    | TupleIndex TextPos Expr Word32
    | Ident      TextPos Symbol
    | Call       TextPos Symbol [Expr]
    | Conv       TextPos Type [Expr]
    | Len        TextPos Expr
    | Copy       TextPos Expr
    | Zero       TextPos
    | Prefix     TextPos Op Expr
    | Infix      TextPos Op Expr Expr
    | Range      TextPos Expr (Maybe Expr) (Maybe Expr)
    | UnsafePtr  TextPos Expr
    deriving (Eq)


data Stmt
    = Assign      TextPos Pattern Expr
    | Set         TextPos Index   Expr
    | Print       TextPos [Expr]
    | CallStmt    TextPos Symbol [Expr]
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Condition Stmt (Maybe Stmt)
    | While       TextPos Condition Stmt
    | FuncDef     TextPos String [Param] Type Stmt
    | Typedef     TextPos Symbol AnnoType
    | AppendStmt  Append
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Symbol (Maybe Type) Expr (Maybe Pattern) Stmt
    deriving (Eq, Show)


data AnnoType
    = AnnoType  Type
    | AnnoTuple [(String, Type)]
    | AnnoADT   [(Symbol, [Type])]
    deriving (Eq)

instance TextPosition Append where
    textPos append = case append of
        AppendTable p _ _ -> p
        AppendIndex i -> textPos i

instance TextPosition Index where
    textPos index = case index of
        IndIdent p _ -> p
        IndArray p _ _ -> p
        IndTuple p _ _ -> p

instance TextPosition Pattern where
    textPos pattern = case pattern of
        PatLiteral   e -> textPos e
        PatIgnore    p -> p
        PatIdent     p _ -> p
        PatTuple     p _ -> p
        PatArray     p _ -> p
        PatGuarded   p _ _ -> p
        PatField     p _ _ -> p
        PatAnnotated pat _ -> textPos pat

instance TextPosition Condition where
    textPos condition = case condition of
        CondExpr e -> textPos e
        

instance TextPosition Expr where
    textPos expr = case expr of
        AExpr      t e -> textPos e
        Int        p _ -> p
        Float      p _ -> p
        Bool       p _ -> p
        Char       p _ -> p
        String     p _ -> p
        Tuple      p _ -> p
        Table      p _ -> p
        Member     p _ _ -> p
        Subscript  p _ _ -> p
        TupleIndex p _ _ -> p
        Ident      p _ -> p
        Call       p _ _ -> p 
        Conv       p _ _ -> p
        Len        p _ -> p
        Zero       p -> p
        Copy       p _ -> p
        Prefix     p _ _ -> p
        Infix      p _ _ _ -> p
        Range      p _ _ _ -> p
        UnsafePtr  p _ -> p


instance TextPosition Stmt where
    textPos stmt = case stmt of
        Assign      p _ _ -> p
        Set         p _ _ -> p
        Print       p _ -> p
        CallStmt    p _ _ -> p
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncDef     p _ _ _ _ -> p
        Typedef     p _ _ -> p
        AppendStmt  a -> textPos a
        Switch      p _ _ -> p
        For         p _ _ _ _ _ -> p

tupStrs, arrStrs, brcStrs :: [String] -> String
tupStrs strs = "(" ++ intercalate ", " strs ++ ")"
arrStrs strs = "[" ++ intercalate ", " strs ++ "]"
brcStrs strs = "{" ++ intercalate ", " strs ++ "}"


instance Show Import where
    show (Import path) = "import " ++ path
    show (ImportC path) = "import_c " ++ path
    show (ImportCMacro macro typ) = "import_c_macro " ++ macro ++ " " ++ show typ

instance Show Param where
    show (Param pos name Void) = show name
    show (Param pos name typ)  = show name ++ " " ++ show typ


instance Show Op where
    show op = case op of
        Plus   -> "+"
        Minus  -> "-"
        Times  -> "*"
        Divide -> "/"
        Modulo -> "%"
        LT     -> "<"
        GT     -> ">"
        LTEq   -> "<="
        GTEq   -> ">="
        EqEq   -> "=="
        OrOr   -> "||"
        AndAnd -> "&&"
        NotEq  -> "!="
        Not    -> "!"

instance Show AnnoType where
    show annoType = case annoType of
        AnnoType t   -> show t
        AnnoTuple xs -> tupStrs $ map (\(s, t) -> show s ++ " " ++ show t) xs
        AnnoADT xs   -> brcStrs $ map (\(s, t) -> show s ++ " " ++ show t) xs

instance Show Pattern where
    show pat = case pat of
        PatLiteral c             -> show c
        PatIgnore pos            -> "_"
        PatIdent pos symbol      -> show symbol
        PatTuple pos ps          -> tupStrs (map show ps)
        PatArray pos ps          -> arrStrs (map show ps)
        PatGuarded pos pat expr  -> show pat ++ " | " ++ show expr
        PatField pos symbol pat  -> show symbol ++ "(" ++ show pat ++ ")"
        PatAnnotated pat typ     -> show pat ++ ":" ++ show typ

instance Show Append where
    show append = case append of
        AppendTable p a e -> show a ++ " <- " ++ show e
        AppendIndex i -> show i


instance Show Condition where
    show (CondExpr expr)      = show expr
    show (CondMatch pat expr) = show pat ++ " <- " ++ show expr


instance Show Index where
    show ind = case ind of
        IndIdent pos symbol   -> show symbol
        IndArray pos idx expr -> show idx ++ "[" ++ show expr ++ "]"
        IndTuple pos idx n    -> show idx ++ "." ++ show n


instance Show Expr where
    show expr = case expr of
        AExpr t e                   -> show e ++ ":" ++ show t
        Int pos n                   -> show n
        Float pos f                 -> show f
        Bool pos b                  -> if b then "true" else "false"
        Char pos c                  -> show c
        String pos s                -> show s
        Tuple pos exprs             -> tupStrs (map show exprs)
        Table pos exprss            -> "[" ++  intercalate "; " (map (intercalate ", " . map show) exprss) ++ "]"
        Member pos expr str         -> show expr ++ "." ++ str
        Subscript pos expr1 expr2   -> show expr1 ++ "[" ++ show expr2 ++ "]"
        TupleIndex pos expr n       -> show expr ++ "." ++ show n
        Ident p s                   -> show s 
        Call pos symbol exprs       -> show symbol ++ tupStrs (map show exprs)
        Conv pos typ exprs          -> show typ ++ tupStrs (map show exprs)
        Len pos expr                -> "len(" ++ show expr ++ ")"
        Zero pos                    -> "zero()"
        Copy pos expr               -> "copy(" ++ show expr ++ ")"
        UnsafePtr pos expr          -> "unsafe_ptr(" ++ show expr ++ ")"
        Prefix pos op expr          -> show op ++ show expr
        Infix pos op expr1 expr2    -> show expr1 ++ " " ++ show op ++ " " ++ show expr2
        Range pos expr mexpr1 mexpr2 -> show expr ++ "[" ++ maybe "" show mexpr1 ++ ".." ++ maybe "" show mexpr2 ++ "]"


-- every function must end on a newline and print pre before every line
prettyAST :: AST -> IO ()
prettyAST ast = do
    when (isJust $ astModuleName ast) $
        putStrLn $ "module " ++ (fromJust $ astModuleName ast)

    putStrLn ""

    forM_ (astImports ast) $ \imp ->
        putStrLn $ show imp

    putStrLn ""

    mapM_ (prettyStmt "") (astStmts ast)
    where
        prettyStmt :: String -> Stmt -> IO ()
        prettyStmt pre stmt = case stmt of
            FuncDef pos sym params retty blk -> do
                putStrLn $ pre ++ "fn " ++ sym ++ tupStrs (map show params) ++ " " ++ if retty == Void then "" else show retty
                prettyStmt (pre ++ "\t") blk
                putStrLn ""

            Assign pos pat expr        -> putStrLn $ pre ++ "let " ++ show pat ++ " = " ++ show expr
            Set pos ind expr           -> putStrLn $ pre ++ show ind ++ " = " ++ show expr
            Print pos exprs            -> putStrLn $ pre ++ "print" ++ tupStrs (map show exprs)
            Return pos mexpr -> putStrLn $ pre ++ "return " ++ maybe "" show mexpr
            AppendStmt app -> putStrLn $ pre ++ show app
 
            If pos cnd true mfalse -> do
                putStrLn $ pre ++ "if " ++ show cnd
                prettyStmt (pre ++ "\t") true
                putStrLn $ pre ++ "else"
                maybe (return ()) (prettyStmt (pre ++ "\t")) mfalse

            CallStmt pos symbol exprs -> putStrLn $ pre ++ show symbol ++ tupStrs (map show exprs)
                    

            Block stmts -> do
                mapM_ (prettyStmt pre) stmts

            While pos cnd stmt -> do
                putStrLn $ pre ++ "while " ++ show cnd
                prettyStmt (pre ++ "\t") stmt

            Typedef pos symbol anno -> do
                putStrLn $ pre ++ "typedef " ++ show symbol ++ " " ++ show anno

            Switch pos expr cases -> do
                putStrLn $ pre ++ "switch " ++ show expr
                forM_ cases $ \(pat, stmt) -> do
                    putStrLn $ pre ++ "\t" ++ show pat
                    prettyStmt (pre ++ "\t\t") stmt

            For pos symbol _ expr mcnd blk -> do
                let cndStr = maybe "" ((" | " ++) . show) mcnd
                let exprStr = " " ++ show expr
                putStrLn $ pre ++ "for " ++ "[" ++ show symbol ++ "]" ++ exprStr ++ cndStr
                prettyStmt (pre ++ "\t") blk

