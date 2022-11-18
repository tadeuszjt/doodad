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
    | AppendIndex Expr
    deriving (Eq)

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
    | PatNull      TextPos
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
    | Null       TextPos 
    | String     TextPos String
    | Tuple      TextPos [Expr]
    | Table      TextPos [[Expr]]
    | Field      TextPos Expr String
    | Subscript  TextPos Expr Expr
    | TupleIndex TextPos Expr Word32
    | Ident      TextPos Symbol
    | Call       TextPos Symbol [Expr]
    | Conv       TextPos Type [Expr]
    | Len        TextPos Expr
    | Push       TextPos Expr [Expr]
    | Pop        TextPos Expr [Expr]
    | Copy       TextPos Expr
    | Zero       TextPos
    | Prefix     TextPos Op Expr
    | Infix      TextPos Op Expr Expr
    | Range      TextPos Expr (Maybe Expr) (Maybe Expr)
    | UnsafePtr  TextPos Expr
    | ADT        TextPos Expr
    | CallMember TextPos Expr Symbol [Expr]
    deriving (Eq)


data Stmt
    = Assign      TextPos Pattern Expr
    | Set         TextPos Expr   Expr
    | Print       TextPos [Expr]
    | ExprStmt    TextPos Expr
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Condition Stmt (Maybe Stmt)
    | While       TextPos Condition Stmt
    | FuncDef     TextPos (Maybe Param) String [Param] Type Stmt
    | Typedef     TextPos Symbol AnnoType
    | AppendStmt  Append
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Symbol (Maybe Type) Expr (Maybe Pattern) Stmt
    | Data        TextPos Symbol Type
    deriving (Eq, Show)


data AnnoADTField
    = ADTFieldMember Symbol [Type]
    | ADTFieldType   Type
    | ADTFieldNull
    deriving (Eq)


data AnnoType
    = AnnoType  Type
    | AnnoTuple [(String, Type)]
    | AnnoADT   [AnnoADTField]
    deriving (Eq)


instance TextPosition Append where
    textPos append = case append of
        AppendTable p _ _ -> p
        AppendIndex i -> textPos i

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
        Null       p -> p
        String     p _ -> p
        Tuple      p _ -> p
        Table      p _ -> p
        Field     p _ _ -> p
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
        ADT        p _ -> p
        CallMember p _ _ _ -> p


instance TextPosition Stmt where
    textPos stmt = case stmt of
        Assign      p _ _ -> p
        Set         p _ _ -> p
        Print       p _ -> p
        ExprStmt    p _ -> p
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncDef     p _ _ _ _ _ -> p
        Typedef     p _ _ -> p
        AppendStmt  a -> textPos a
        Switch      p _ _ -> p
        For         p _ _ _ _ _ -> p
        Data        p _ _ -> p

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

instance Show AnnoADTField where
    show annoAdtField = case annoAdtField of
        ADTFieldType typ -> show typ
        ADTFieldMember sym ts -> show sym ++ tupStrs (map show ts)

instance Show AnnoType where
    show annoType = case annoType of
        AnnoType t   -> show t
        AnnoTuple xs -> tupStrs $ map (\(s, t) -> show s ++ " " ++ show t) xs
        AnnoADT xs   -> brcStrs $ map show xs

instance Show Pattern where
    show pat = case pat of
        PatLiteral c             -> show c
        PatIgnore pos            -> "_"
        PatIdent pos symbol      -> show symbol
        PatTuple pos ps          -> tupStrs (map show ps)
        PatArray pos ps          -> arrStrs (map show ps)
        PatGuarded pos pat expr  -> show pat ++ " | " ++ show expr
        PatField pos symbol pats -> show symbol ++ tupStrs (map show pats)
        PatTypeField pos typ pat -> show typ ++ tupStrs [show pat]
        PatAnnotated pat typ     -> show pat ++ ":" ++ show typ
        PatNull pos              -> "null"

instance Show Append where
    show append = case append of
        AppendTable p a e -> show a ++ " <- " ++ show e
        AppendIndex i -> show i


instance Show Condition where
    show (CondExpr expr)      = show expr
    show (CondMatch pat expr) = show pat ++ " <- " ++ show expr


instance Show Expr where
    show expr = case expr of
        AExpr t e                   -> show e ++ ":" ++ show t
        Int pos n                   -> show n
        Float pos f                 -> show f
        Bool pos b                  -> if b then "true" else "false"
        Char pos c                  -> show c
        Null p                      -> "null"
        String pos s                -> show s
        Tuple pos exprs             -> tupStrs (map show exprs)
        Table pos exprss            -> "[" ++  intercalate "; " (map (intercalate ", " . map show) exprss) ++ "]"
        Field pos expr str         -> show expr ++ "." ++ str
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
        ADT pos expr                 -> brcStrs [show expr]
        CallMember pos expr symbol exprs -> show expr ++ "." ++ show symbol ++ tupStrs (map show exprs)


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
            FuncDef pos mparam sym params retty blk -> do
                paramStr <- case mparam of
                    Nothing            -> return ""
                    Just (Param _ s t) -> return $ "(" ++ show s ++ " " ++ show t ++ ") "
                
                putStrLn $ pre ++ "fn " ++ paramStr ++ sym ++ tupStrs (map show params) ++ " " ++ if retty == Void then "" else show retty
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

            ExprStmt pos callExpr -> putStrLn $ pre ++ show callExpr
                    

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

            Data pos symbol typ -> do
                putStrLn $ pre ++ "data " ++ show symbol ++ " " ++ show typ

            _  -> error $ "invalid stmt: " ++ show stmt

