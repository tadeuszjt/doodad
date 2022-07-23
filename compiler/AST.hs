module AST where

import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Type
import Error

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
    = AExpr      Type    Expr
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
    | Prefix     TextPos Op Expr
    | Infix      TextPos Op Expr Expr
    | Range      TextPos Expr (Maybe Expr) (Maybe Expr)
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
    | For         TextPos Symbol (Maybe Type) (Maybe Expr) (Maybe Condition) Stmt
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

instance TextPosition Condition where
    textPos condition = case condition of
        CondExpr e -> textPos e
        

instance TextPosition Expr where
    textPos expr = case expr of
        AST.AExpr      t e -> textPos e
        AST.Int        p _ -> p
        AST.Float      p _ -> p
        AST.Bool       p _ -> p
        AST.Char       p _ -> p
        AST.String     p _ -> p
        AST.Tuple      p _ -> p
        AST.Table      p _ -> p
        AST.Member     p _ _ -> p
        AST.Subscript  p _ _ -> p
        AST.TupleIndex p _ _ -> p
        AST.Ident      p _ -> p
        AST.Call       p _ _ -> p 
        AST.Conv       p _ _ -> p
        AST.Len        p _ -> p
        AST.Copy       p _ -> p
        AST.Prefix     p _ _ -> p
        AST.Infix      p _ _ _ -> p
        AST.Range      p _ _ _ -> p


instance TextPosition Stmt where
    textPos stmt = case stmt of
        AST.Assign      p _ _ -> p
        AST.Set         p _ _ -> p
        AST.Print       p _ -> p
        AST.CallStmt    p _ _ -> p
        AST.Return      p _ -> p
        AST.Block       s -> textPos (head s)
        AST.If          p _ _ _ -> p
        AST.While       p _ _ -> p
        AST.FuncDef     p _ _ _ _ -> p
        AST.Typedef     p _ _ -> p
        AST.AppendStmt  a -> textPos a
        AST.Switch      p _ _ -> p
        AST.For         p _ _ _ _ _ -> p

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
        AST.Plus   -> "+"
        AST.Minus  -> "-"
        AST.Times  -> "*"
        AST.Divide -> "/"
        AST.Modulo -> "%"
        AST.LT     -> "<"
        AST.GT     -> ">"
        AST.LTEq   -> "<="
        AST.GTEq   -> ">="
        AST.EqEq   -> "=="
        AST.OrOr   -> "||"
        AST.AndAnd -> "&&"
        AST.NotEq  -> "!="
        AST.Not    -> "!"

instance Show AnnoType where
    show annoType = case annoType of
        AnnoType t   -> show t
        AnnoTuple xs -> tupStrs $ map (\(s, t) -> show s ++ " " ++ show t) xs
        AnnoADT xs   -> brcStrs $ map (\(s, t) -> show s ++ " " ++ show t) xs

instance Show Pattern where
    show pat = case pat of
        PatLiteral c     -> show c
        PatIgnore pos    -> "_"
        PatIdent pos symbol -> show symbol
        PatTuple pos ps  -> tupStrs (map show ps)
        PatArray pos ps  -> arrStrs (map show ps)
        PatGuarded pos pat expr -> show pat ++ " | " ++ show expr
        PatField pos symbol pat -> show symbol ++ "(" ++ show pat ++ ")"

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
        AST.AExpr t e                   -> show e ++ ":" ++ show t
        AST.Int pos n                   -> show n
        AST.Float pos f                 -> show f
        AST.Bool pos b                  -> if b then "true" else "false"
        AST.Char pos c                  -> show c
        AST.String pos s                -> show s
        AST.Tuple pos exprs             -> tupStrs (map show exprs)
        AST.Table pos exprss            -> "[" ++  intercalate "; " (map (intercalate ", " . map show) exprss) ++ "]"
        AST.Member pos expr str         -> show expr ++ "." ++ str
        AST.Subscript pos expr1 expr2   -> show expr1 ++ "[" ++ show expr2 ++ "]"
        AST.TupleIndex pos expr n       -> show expr ++ "." ++ show n
        AST.Ident p s                   -> show s 
        AST.Call pos symbol exprs       -> show symbol ++ tupStrs (map show exprs)
        AST.Conv pos typ exprs          -> show typ ++ tupStrs (map show exprs)
        AST.Len pos expr                -> "len(" ++ show expr ++ ")"
        AST.Copy pos expr               -> "copy(" ++ show expr ++ ")"
        AST.Prefix pos op expr          -> show op ++ show expr
        AST.Infix pos op expr1 expr2    -> show expr1 ++ " " ++ show op ++ " " ++ show expr2
        AST.Range pos expr mexpr1 mexpr2 -> show expr ++ "[" ++ maybe "" show mexpr1 ++ ".." ++ maybe "" show mexpr2 ++ "]"


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

            AST.Typedef pos symbol anno -> do
                putStrLn $ pre ++ "typedef " ++ show symbol ++ " " ++ show anno

            Switch pos expr cases -> do
                putStrLn $ pre ++ "switch " ++ show expr
                forM_ cases $ \(pat, stmt) -> do
                    putStrLn $ pre ++ "\t" ++ show pat
                    prettyStmt (pre ++ "\t\t") stmt

            For pos symbol _ mexpr mcnd blk -> do
                let cndStr = maybe "" ((" | " ++) . show) mcnd
                let exprStr = maybe "" ((" " ++ ) . show) mexpr
                putStrLn $ pre ++ "for " ++ "[" ++ show symbol ++ "]" ++ exprStr ++ cndStr
                prettyStmt (pre ++ "\t") blk

