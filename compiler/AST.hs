module AST where

import Prelude hiding (LT, GT)
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Type (Type, Type(Void), Typeof, typeof)
import Error
import Symbol

data AST
    = AST
        { astModuleName :: String
        , astImports    :: [Import]
        , astStmts      :: [Stmt]
        }
    deriving (Eq)


data Import
    = Import FilePath
    | CInclude String
    | CLink String
    deriving (Eq, Ord)


data Param
    = Param
        { paramPos  :: TextPos
        , paramSymbol :: Symbol
        , paramType :: Type
        }
    | RefParam
        { paramPos :: TextPos
        , paramSymbol :: Symbol
        , paramType :: Type
        }
    deriving (Eq, Ord)


data Retty
    = Retty Type
    | RefRetty Type
    deriving (Eq, Ord)


instance Type.Typeof Param where
    typeof = paramType

instance Type.Typeof Retty where
    typeof (Retty t) = t
    typeof (RefRetty t) = t


data Pattern
    = PatLiteral   Expr                   -- equal(a, b)
    | PatIgnore    TextPos                -- fine
    | PatIdent     TextPos Symbol         -- set(&ident, b)
    | PatTuple     TextPos [Pattern]      -- set(&a, first(&z)); set(&b, second(&z)) ...
    | PatSlice     TextPos [Pattern]      
    | PatGuarded   TextPos Pattern Expr   -- fine
    | PatAnnotated Pattern Type           -- fine
    | PatTypeField TextPos Type [Pattern] -- I64(x) 
    | PatField     TextPos Symbol Pattern -- just(y) | x.isJust
    deriving (Eq)

data Expr
    = AExpr        Type  Expr
    | Int          TextPos Integer
    | Float        TextPos Double
    | Bool         TextPos Bool
    | Char         TextPos Char
    | String       TextPos String
    | Call         TextPos Symbol [Expr]
    | Field        TextPos Expr Int
    | Ident        TextPos Symbol
    | Builtin      TextPos String [Expr]
    | Match        TextPos Expr Pattern
    | Reference    TextPos Expr
    | Array        TextPos [Expr]
    deriving (Eq)

instance Typeof Expr where
    typeof (AExpr typ _) = typ
    typeof a = error $ "can only take typeof AExpr: " ++ show a

instance Typeof Pattern where
    typeof (PatAnnotated _ typ) = typ
    typeof a = error $ "can only take typeof PatAnnotated" 


data FuncHeader
    = FuncHeader
        { funcPos :: TextPos
        , funcGenerics :: [Symbol]
        , funcSymbol :: Symbol
        , funcArgs :: [Param]
        , funcRetty :: Retty
        }
    deriving (Eq, Show)


data Stmt
    = Let         TextPos Pattern (Maybe Expr) (Maybe Stmt)
    | ExprStmt    Expr
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Expr Stmt (Maybe Stmt)
    | While       TextPos Expr Stmt
    | FuncDef     FuncHeader Stmt
    | Feature     TextPos [Symbol] Symbol Type [FuncHeader]
    | Typedef     TextPos [Symbol] Symbol AnnoType
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Expr (Maybe Pattern) Stmt
    | Data        TextPos Symbol Type (Maybe Expr)
    | EmbedC      TextPos String
    deriving (Eq, Show)


data AnnoType
    = AnnoType   Type
    | AnnoTuple  [Param]
    | AnnoTable  [Param]
    | AnnoApply  Symbol [Param]
    deriving (Eq)


instance TextPosition Pattern where
    textPos pattern = case pattern of
        PatLiteral   e -> textPos e
        PatIgnore    p -> p
        PatIdent     p _ -> p
        PatTuple     p _ -> p
        PatGuarded   p _ _ -> p
        PatTypeField p _ _ -> p
        PatSlice     p _ -> p
        PatAnnotated pat _ -> textPos pat


instance TextPosition Expr where
    textPos expression = case expression of
        AExpr        t expr -> textPos expr
        Int          p _    -> p
        Float        p _    -> p
        Bool         p _    -> p
        Char         p _    -> p
        String       p _    -> p
        Field        p _ _  -> p
        Ident        p _    -> p
        Call         p _ _ -> p 
        Builtin      p _ _ -> p 
        Match        p _ _ -> p
        Reference    p _ -> p
        Array        p _ -> p
        _ -> error (show expression)

instance TextPosition FuncHeader where
    textPos (FuncHeader pos _ _ _ _) = pos

instance TextPosition Stmt where
    textPos stmt = case stmt of
        ExprStmt    expr -> textPos expr
        Let         p _ _ _ -> p
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncDef     h _ -> textPos h
        Typedef     p _ _ _ -> p
        Switch      p _ _ -> p
        For         p _ _ _ -> p
        Data        p _ _ _ -> p
        EmbedC      p _ -> p
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
    show (Param pos name typ)    = show name ++ " " ++ show typ
    show (RefParam pos name typ) = show name ++ "& " ++ show typ


instance Show Retty where
    show (Retty Void) = ""
    show (Retty t) = show t
    show (RefRetty t) = "&" ++ show t


instance Show AnnoType where
    show annoType = case annoType of
        AnnoType t   -> show t
        AnnoTuple ps -> tupStrs $ map show ps
        AnnoTable xs -> "Table{" ++ intercalate ", " (map show xs) ++ "}"


instance Show Pattern where
    show pattern = case pattern of
        PatLiteral c             -> show c
        PatIgnore pos            -> "_"
        PatIdent pos symbol      -> show symbol
        PatTuple pos ps          -> tupStrs (map show ps)
        PatGuarded pos pat expr  -> show pat ++ " | " ++ show expr
        PatTypeField pos typ pats    -> show typ ++ tupStrs (map show pats)
        PatAnnotated pat typ     -> show pat ++ ":" ++ show typ
        PatField _ symbol pat    -> show symbol ++ tupStrs [show pat]
        PatSlice _ pats          -> arrStrs (map show pats)


instance Show Expr where
    show expression = case expression of
        AExpr t expr                       -> show expr ++ ":" ++ show t 
        Int pos n                          -> show n
        Float pos f                        -> show f
        Bool pos b                         -> if b then "true" else "false"
        Char pos c                         -> show c
        String pos s                       -> show s
        Array pos exprs                    -> arrStrs (map show exprs)
        Field pos expr symbol              -> show expr ++ "." ++ show symbol
        Ident p s                          -> show s 
        Call pos symbol exprs              -> show symbol ++ tupStrs (map show exprs)
        Builtin pos sym exprs              -> sym ++ tupStrs (map show exprs)
        Match pos expr1 expr2              -> "(" ++ show expr1 ++ " -> " ++ show expr2 ++ ")"
        Reference pos expr                 -> "&" ++ show expr


-- every function must end on a newline and print pre before every line
prettyAST :: AST -> IO ()
prettyAST ast = do
    putStrLn $ "module " ++ (astModuleName ast)
    putStrLn ""
    forM_ (astImports ast) $ \imp ->
        putStrLn $ show imp
    putStrLn ""
    mapM_ (prettyStmt "") (astStmts ast)


prettyFuncHeader :: String -> FuncHeader -> IO ()
prettyFuncHeader pre (FuncHeader pos generics symbol args retty) = do
    genericsStr <- case generics of
        [] -> return ""
        as -> return $ brcStrs (map show as)
    putStrLn $ pre
        ++ "fn"
        ++ genericsStr
        ++ " "
        ++ show symbol
        ++ tupStrs (map show args)
        ++ " "
        ++ show retty

prettyStmt :: String -> Stmt -> IO ()
prettyStmt pre stmt = case stmt of
    FuncDef header blk -> do
        prettyFuncHeader pre header
        prettyStmt (pre ++ "\t") blk
        putStrLn ""

    Let pos pat expr mblk -> do
        putStrLn $ pre ++ "let " ++ show pat ++ " = " ++ show expr ++ if isJust mblk then " in" else ""
        when (isJust mblk) $ prettyStmt (pre ++ "\t") (fromJust mblk)

    Return pos mexpr       -> putStrLn $ pre ++ "return " ++ maybe "" show mexpr

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

