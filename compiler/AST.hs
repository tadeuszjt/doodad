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
        , astImports    :: [FilePath]
        , astStmts      :: [Stmt]
        }


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


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: String
        , paramType :: Type
        }
    deriving (Eq)
instance Show Param where
    show (Param pos name typ) = name ++ " " ++ show typ


data Append
    = AppendTable TextPos Append Expr
    | AppendElem TextPos Append Expr
    | AppendIndex Index
    deriving (Show, Eq)

data Pattern
    = PatLiteral   Expr
    | PatIgnore    TextPos
    | PatIdent     TextPos String
    | PatTuple     TextPos [Pattern]
    | PatArray     TextPos [Pattern]
    | PatGuarded   TextPos Pattern Expr
    | PatTyped     TextPos Type [Pattern]
    | PatSplit     TextPos Pattern Pattern
    | PatSplitElem TextPos Pattern Pattern
    deriving (Eq)

data Index
    = IndIdent TextPos String
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
    | Null       TextPos
    | String     TextPos String
    | Tuple      TextPos [Expr]
    | Array      TextPos [Expr]
    | Table      TextPos [[Expr]]
    | Member     TextPos Expr String
    | Subscript  TextPos Expr Expr
    | Range      TextPos Expr (Maybe Expr) (Maybe Expr)
    | TupleIndex TextPos Expr Word32
    | Ident      TextPos Symbol
    | Call       TextPos Symbol [Expr]
    | CallExpr   TextPos Expr [Expr]
    | Conv       TextPos Type [Expr]
    | Len        TextPos Expr
    | Copy       TextPos Expr
    | Prefix     TextPos Op Expr
    | Infix      TextPos Op Expr Expr
    | Expr       Int
    deriving (Eq)


data Stmt
    = Assign   TextPos Pattern Expr
    | Set      TextPos Index   Expr
    | Print    TextPos [Expr]
    | CallStmt TextPos Index  [Expr]
    | Return   TextPos (Maybe Expr)
    | Block    [Stmt]
    | If       TextPos Condition Stmt (Maybe Stmt)
    | While    TextPos Condition Stmt
    | For      TextPos String Expr (Maybe Expr) Stmt
    | Switch   TextPos Expr [(Pattern, Stmt)]
    | FuncDef  TextPos String [Param] Type Stmt
    | Extern   TextPos String String [Param] Type
    | Typedef  TextPos String AnnoType
    | AppendStmt Append
    deriving (Eq, Show)


data AnnoType
    = AnnoType  Type
    | AnnoTuple [(String, Type)]
    | AnnoADT   [(String, Type)]
    deriving (Eq, Show)



instance TextPosition Append where
    textPos append = case append of
        AppendTable p _ _ -> p
        AppendElem p _ _ -> p
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
        PatTyped     p _ _ -> p
        PatSplit     p _ _ -> p
        PatSplitElem p _ _ -> p
        

instance TextPosition Expr where
    textPos expr = case expr of
        AST.Int        p _ -> p
        AST.Float      p _ -> p
        AST.Bool       p _ -> p
        AST.Char       p _ -> p
        AST.Null       p -> p
        AST.String     p _ -> p
        AST.Tuple      p _ -> p
        AST.Array      p _ -> p
        AST.Table      p _ -> p
        AST.Member     p _ _ -> p
        AST.Subscript  p _ _ -> p
        AST.Range      p _ _ _ -> p
        AST.TupleIndex p _ _ -> p
        AST.Ident      p _ -> p
        AST.Call       p _ _ -> p 
        AST.CallExpr   p _ _ -> p
        AST.Conv       p _ _ -> p
        AST.Len        p _ -> p
        AST.Copy       p _ -> p
        AST.Prefix     p _ _ -> p
        AST.Infix      p _ _ _ -> p
        AST.Expr       _ -> error "Cannot take text position"


instance TextPosition Stmt where
    textPos stmt = case stmt of
        AST.Assign     p _ _ -> p
        AST.Set        p _ _ -> p
        AST.Print      p _ -> p
        AST.CallStmt   p _ _ -> p
        AST.Return     p _ -> p
        AST.Block      s -> textPos (head s)
        AST.If         p _ _ _ -> p
        AST.While      p _ _ -> p
        AST.For        p _ _ _ _ -> p
        AST.Switch     p _ _ -> p
        AST.FuncDef    p _ _ _ _ -> p
        AST.Extern     p _ _ _ _ -> p
        AST.Typedef    p _ _ -> p
        AST.AppendStmt a -> textPos a

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


instance Show Pattern where
    show pat = case pat of
        PatLiteral c     -> show c
        PatIgnore pos    -> "_"
        PatIdent pos s   -> s
        PatTuple pos ps  -> tupStrs (map show ps)
        PatArray pos ps  -> arrStrs (map show ps)
        PatGuarded pos pat expr -> show pat ++ " | " ++ show expr
        PatTyped pos s p -> show s ++ "(" ++ show p ++ ")"
        PatSplit pos a b -> show a ++ " ->> " ++ show b
        PatSplitElem pos a b -> show a ++ " -> " ++ show b


instance Show Condition where
    show (CondExpr expr) = show expr
    show (CondMatch pat expr) = show pat ++ " <- " ++ show expr


instance Show Index where
    show ind = case ind of
        IndIdent pos str      -> str
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
        AST.Array pos exprs             -> "[ |" ++ intercalate ", " (map show exprs) ++ "]"
        AST.Table pos exprss            -> "[" ++  intercalate "; " (map (intercalate ", " . map show) exprss) ++ "]"
        AST.Member pos expr str         -> show expr ++ "." ++ str
        AST.Subscript pos expr1 expr2   -> show expr1 ++ "[" ++ show expr2 ++ "]"
        AST.Range pos expr mLeft mRight -> "[" ++ maybe "" show mLeft ++ ".." ++ maybe "" show mRight ++ "]"
        AST.TupleIndex pos expr n       -> show expr ++ "." ++ show n
        AST.Ident p s                   -> show s 
        AST.Call pos symbol exprs       -> show symbol ++ tupStrs (map show exprs)
        AST.CallExpr pos expr exprs     -> show expr ++ tupStrs (map show exprs)
        AST.Conv pos typ exprs          -> show typ ++ tupStrs (map show exprs)
        AST.Len pos expr                -> "len(" ++ show expr ++ ")"
        AST.Copy pos expr               -> "copy(" ++ show expr ++ ")"
        AST.Prefix pos op expr          -> show op ++ show expr
        AST.Infix pos op expr1 expr2    -> show expr1 ++ " " ++ show op ++ " " ++ show expr2
        AST.Null pos                    -> "null"
        AST.Expr n                      -> "e" ++ show n


prettyAST :: String -> AST -> IO ()
prettyAST pre ast = do
    when (isJust $ astModuleName ast) $
        putStrLn $ pre ++ "module " ++ (fromJust $ astModuleName ast)

    putStrLn ""
    
    forM_ (astImports ast) $ \path ->
        putStrLn $ "import " ++ show path

    putStrLn ""


    forM_ (astStmts ast) $ \stmt -> prettyStmt pre stmt >> putStrLn ""
    where
        prettyStmt :: String -> Stmt -> IO ()
        prettyStmt pr stmt = case stmt of
            Assign pos pat expr     -> putStrLn ("let " ++ show pat ++ " = " ++ show expr) >> putStr pr
            Set pos ind expr        -> putStrLn (show ind ++ " = " ++ show expr) >> putStr pr
            Print pos exprs         -> putStrLn ("print" ++ tupStrs (map show exprs)) >> putStr pr
            CallStmt pos expr exprs -> putStrLn (show expr ++ tupStrs (map show exprs)) >> putStr pr
            Return pos mexpr        -> putStrLn ("return " ++ maybe "" show mexpr) >> putStr pr

            If pos cnd true false -> do
                putStr $ "if " ++ show cnd
                prettyBlock pr true

            FuncDef pos symbol params mretty blk -> do
                putStr $ "fn " ++ show symbol ++ tupStrs (map show params) ++ " " ++ if mretty == Void then "" else show mretty
                prettyBlock pr blk

            Switch pos cnd cases -> do
                putStrLn ("switch " ++ show cnd)
                putStr (pr ++ "\t")
                
                forM_ cases $ \(c, blk) -> do
                    putStr (show c)
                    prettyBlock (pr ++ "\t") blk
                    
                putStrLn "" >> putStr pr

            Block stmts -> do
                putStr "\t"
                mapM_ (prettyStmt (pr ++ "\t")) stmts
                putStrLn ""
                putStr pr
                



            AppendStmt app -> putStrLn "append" >> putStr pr

            For pos istr expr (mexpr) stmt -> do
                putStr $ "for [" ++ istr ++ "] " ++ show expr ++ maybe "" ((" | " ++) . show) mexpr
                prettyBlock pr stmt

            _ -> error $ "Cannot pretty: " ++ show stmt

        prettyBlock :: String -> Stmt -> IO ()
        prettyBlock pr stmt = case stmt of
            Block []    -> putStrLn "" >> putStr pr
            Block stmts -> do
                putStrLn ""
                putStr (pr ++ "\t")

                forM_ stmts $ \stmt ->
                    prettyStmt (pr ++ "\t") stmt

                putStrLn "" >> putStr pr


            _           -> putStr "; " >> prettyStmt pr stmt

