module AST where

import           Data.Word
import           Type
import           Error
import qualified Lexer    as L


data AST =
    AST { astStmts :: [Stmt] }
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
    deriving (Show, Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramName :: String
        , paramType :: Type
        }
    deriving (Show, Eq)


data Data
    = DataIdent { dataPos :: TextPos, dataSymbol :: String }
    | DataFunc  { dataPos :: TextPos, dataSymbol :: String, dataParams :: [Param] }
    deriving (Show, Eq)


data Pattern
    = PatLiteral Constant
    | PatIgnore  TextPos
    | PatIdent   TextPos String
    | PatTuple   TextPos [Pattern]
    | PatArray   TextPos [Pattern]
    | PatTyped   TextPos String Pattern
    deriving (Show, Eq)


data Index
    = IndIdent TextPos String
    | IndArray TextPos Index Expr
    | IndTuple TextPos Index Word32
    deriving (Show, Eq)


data Constant
    = Int TextPos Integer
    | Float TextPos Double
    | Bool TextPos Bool
    | Char TextPos Char
    | String TextPos String
    deriving (Show, Eq)


data Expr
    = Expr Int
    | Cons Constant
    | Tuple TextPos [Expr]
    | Array TextPos [Expr]
    | Table TextPos [[Expr]]
    | Member TextPos Expr String
    | Subscript TextPos Expr Expr
    | TupleIndex TextPos Expr Word32
    | Ident TextPos String
    | Call TextPos String [Expr]
    | Conv TextPos Type [Expr]
    | Len TextPos Expr
    | Append TextPos Expr Expr
    | Prefix TextPos Op Expr
    | Infix TextPos Op Expr Expr
    deriving (Show, Eq)


data Stmt
    = Stmt Int
    | Assign TextPos Pattern Expr
    | Set TextPos Index Expr
    | Print TextPos [Expr]
    | CallStmt TextPos String [Expr]
    | Return TextPos (Maybe Expr)
    | Block TextPos [Stmt]
    | If TextPos Expr Stmt (Maybe Stmt)
    | While  TextPos Expr [Stmt]
    | Switch TextPos Expr [(Pattern, Stmt)]
    | Func TextPos String [Param] (Maybe Type) [Stmt]
    | Extern TextPos String [Param] (Maybe Type)
    | Typedef TextPos String Type
    | Datadef TextPos String [Data] 
    | ModuleName TextPos String
    deriving (Show, Eq)


prettyAST :: AST -> IO ()
prettyAST ast =
    mapM_ (prettyStmt "") (astStmts ast)
    where
        prettyStmt :: String -> Stmt -> IO ()
        prettyStmt preppend stmt = case stmt of
            Block pos stmts -> do
                putStrLn (preppend ++ "block")
                mapM_ (prettyStmt (preppend ++ "\t")) stmts

            If pos cnd true false -> do
                putStr (preppend ++ "if")
                prettyStmt (preppend ++ "\t") true
                maybe (return ()) (prettyStmt (preppend ++ "\t")) false

            While pos cnd stmts -> do
                putStrLn (preppend ++ "while " ++ show cnd)
                mapM_ (prettyStmt (preppend ++ "\t")) stmts

            Func pos symbol params mretty stmts -> do
                putStrLn (preppend ++ "func " ++ show symbol ++ show params ++ show mretty)
                mapM_ (prettyStmt (preppend ++ "\t")) stmts

            _ -> putStrLn (preppend ++ show stmt)

