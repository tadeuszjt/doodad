module AST where

import           Data.Word
import           Data.List
import           Control.Monad
import           Type
import           Error
import qualified Lexer    as L
import qualified Data.Set as Set

type ModuleName = String
type Symbol     = String
type Path       = [String]


data AST =
    AST { astModuleName :: Maybe ModuleName
        , astImports    :: [Path]
        , astStmts      :: [Stmt]
        }
    deriving (Show)


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
    deriving (Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: Symbol
        , paramType :: Type
        }
    deriving (Eq)
instance Show Param where
    show (Param pos name typ) = name ++ " " ++ show typ


data Pattern
    = PatLiteral Constant
    | PatIgnore  TextPos
    | PatIdent   TextPos Symbol
    | PatTuple   TextPos [Pattern]
    | PatArray   TextPos [Pattern]
    | PatTyped   TextPos Symbol Pattern
    deriving (Eq)


data Index
    = IndIdent TextPos Symbol
    | IndArray TextPos Index Expr
    | IndTuple TextPos Index Word32
    deriving (Eq)


data Constant
    = Int TextPos Integer
    | Float TextPos Double
    | Bool TextPos Bool
    | Char TextPos Char
    | String TextPos String
    deriving (Eq)


data Expr
    = Cons Constant
    | Tuple TextPos [Expr]
    | Array TextPos [Expr]
    | Table TextPos [[Expr]]
    | Member TextPos Expr Symbol
    | Subscript TextPos Expr Expr
    | TupleIndex TextPos Expr Word32
    | Ident TextPos Symbol
    | Call TextPos Symbol [Expr]
    | Conv TextPos Type [Expr]
    | Len TextPos Expr
    | Append TextPos Expr Expr
    | Prefix TextPos Op Expr
    | Infix TextPos Op Expr Expr
    deriving (Eq)


data Stmt
    = Assign TextPos Pattern Expr
    | Set TextPos Index Expr
    | Print TextPos [Expr]
    | CallStmt TextPos Symbol [Expr]
    | Return TextPos (Maybe Expr)
    | Block TextPos [Stmt]
    | If TextPos Expr Stmt (Maybe Stmt)
    | While  TextPos Expr [Stmt]
    | Switch TextPos Expr [(Pattern, Stmt)]
    | Func TextPos Symbol [Param] (Maybe Type) [Stmt]
    | Extern TextPos Symbol [Param] (Maybe Type)
    | Typedef TextPos Symbol Type
    deriving (Show, Eq)


tupStrs, arrStrs, brcStrs :: [String] -> String
tupStrs strs = "(" ++ intercalate ", " strs ++ ")"
arrStrs strs = "[" ++ intercalate ", " strs ++ "]"
brcStrs strs = "{" ++ intercalate ", " strs ++ "}"


instance Show Op where
    show op = case op of
        AST.Plus   -> "+"
        AST.Minus  -> "-"
        AST.Times  -> "*"
        AST.Divide -> "/"
        AST.Mod    -> "%"
        AST.LT     -> "<"
        AST.GT     -> ">"
        AST.LTEq   -> "<="
        AST.GTEq   -> ">="
        AST.EqEq   -> "=="
        AST.OrOr   -> "||"
        AST.AndAnd -> "&&"


instance Show Pattern where
    show pat = case pat of
        PatLiteral c     -> show c
        PatIgnore pos    -> "_"
        PatIdent pos s   -> "PatId(" ++ show s ++ ")"
        PatTuple pos ps  -> tupStrs (map show ps)
        PatArray pos ps  -> arrStrs (map show ps)
        PatTyped pos s p -> show s ++ ":" ++ show p


instance Show Index where
    show ind = case ind of
        IndIdent pos str      -> "IndIdent(" ++ show str ++ ")"
        IndArray pos idx expr -> "IndArray(" ++ show idx ++ ", " ++ show expr ++ ")"
        IndTuple pos idx n    -> "IndTuple(" ++ show idx ++ ", " ++ show n ++ ")"


instance Show Constant where
    show cons = case cons of
        AST.Int pos n    -> show n
        AST.Float pos f  -> show f
        AST.Bool pos b   -> show b
        AST.Char pos c   -> show c
        AST.String pos s -> show s


instance Show Expr where
    show expr = case expr of
        AST.Cons c                    -> show c
        AST.Tuple pos exprs           -> "Tuple" ++ tupStrs (map show exprs) 
        AST.Array pos exprs           -> "Array" ++ arrStrs (map show exprs)
        AST.Table pos exprss          -> "Table" ++ brcStrs (map show (map (AST.Array pos) exprss))
        AST.Member pos expr str       -> "Member" ++ tupStrs [show expr, show str]
        AST.Subscript pos expr1 expr2 -> "Subscript" ++ tupStrs [show expr1, show expr2]
        AST.TupleIndex pos expr n     -> "Index" ++ tupStrs [show expr, show n]
        AST.Ident pos s               -> "Ident(" ++ show s ++ ")"
        AST.Call pos symbol exprs     -> "Call" ++ tupStrs (show symbol: map show exprs)
        AST.Conv pos typ exprs        -> "Conv" ++ tupStrs (show typ: map show exprs)
        AST.Len pos expr              -> "Len(" ++ show expr ++ ")"
        AST.Append pos expr1 expr2    -> "Append" ++ tupStrs [show expr1, show expr2]
        AST.Prefix pos op expr        -> show op ++ show expr
        AST.Infix pos op expr1 expr2  -> "(" ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2 ++ ")"


prettyAST :: String -> AST -> IO ()
prettyAST pre ast = do
    putStrLn $ pre ++ "Module: " ++ maybe "None" show (astModuleName ast)
    putStrLn $ pre ++ "Imports: " ++ show (astImports ast)
    forM_ (astStmts ast) $ \stmt -> prettyStmt pre stmt >> putStrLn ""
    where
        prettyStmt :: String -> Stmt -> IO ()
        prettyStmt pr stmt = case stmt of
            Assign pos pat expr -> do
                putStrLn (pr ++ "let " ++ show pat ++ " = " ++ show expr)

            Set pos ind expr -> do
                putStrLn (pr ++ show ind ++ " = " ++ show expr)

            Print pos exprs -> do
                putStrLn (pr ++ "Print" ++ tupStrs (map show exprs))

            CallStmt pos symbol exprs -> do
                putStrLn (pr ++ symbol ++ tupStrs (map show exprs))

            Return pos mexpr -> do
                putStrLn (pr ++ "Return " ++ show mexpr)

            Block pos stmts -> do
                putStrLn (pr ++ "block")
                mapM_ (prettyStmt (pr ++ "\t")) stmts

            If pos cnd true false -> do
                putStr (pr ++ "if")
                prettyStmt (pr ++ "\t") true
                maybe (return ()) (prettyStmt (pr ++ "\t")) false

            While pos cnd stmts -> do
                putStrLn (pr ++ "while " ++ show cnd)
                mapM_ (prettyStmt (pr ++ "\t")) stmts

            Func pos symbol params mretty stmts -> do
                putStrLn (pr ++ "Func " ++ symbol ++ tupStrs (map show params) ++ " " ++ show mretty)
                mapM_ (prettyStmt (pr ++ "\t")) stmts

            Extern pos symbol params mretty -> do
                putStrLn (pr ++ "Extern " ++ symbol ++ tupStrs (map show params) ++ " " ++ show mretty)

            Switch pos cnd cases -> do
                putStrLn (pr ++ "switch " ++ show cnd)
                forM_ cases $ \(c, blk) -> do
                    putStrLn (pr ++ "\tcase " ++ show c ++ ":")
                    prettyStmt (pr ++ "\t\t") blk

            _ -> putStrLn (pr ++ show stmt)

