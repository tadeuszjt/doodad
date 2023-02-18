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
        , paramType__ :: Type
        }
    deriving (Eq, Ord)

instance Type.Typeof Param where typeof (Param pos name typ) = typ


data Pattern
    = PatLiteral   Expr
    | PatIgnore    TextPos
    | PatIdent     TextPos Symbol
    | PatTuple     TextPos [Pattern]
    | PatArray     TextPos [[Pattern]]
    | PatGuarded   TextPos Pattern Expr
    | PatField     TextPos Symbol [Pattern]
    | PatTypeField TextPos Type Pattern
    | PatAnnotated Pattern Type
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
    | Initialiser TextPos [Expr]
    | Call        TextPos [Expr] Symbol [Expr]
    | Null        TextPos 
    | Field       TextPos Expr Symbol
    | Subscript   TextPos Expr Expr
    | Ident       TextPos Symbol
    | Conv        TextPos Type [Expr]
    | Builtin     TextPos [Expr] String [Expr]
    | Prefix      TextPos Operator Expr
    | Infix       TextPos Operator Expr Expr
    | ADT         TextPos Expr
    | Match       TextPos Expr Pattern
    | Range       TextPos (Maybe Expr) (Maybe Expr) (Maybe Expr)
    deriving (Eq)


data Stmt
    = Assign      TextPos Pattern Expr
    | Set         TextPos Expr   Expr
    | Print       TextPos [Expr]
    | ExprStmt    Expr
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Expr Stmt (Maybe Stmt)
    | While       TextPos Expr Stmt
    | FuncDef     TextPos [Param] Symbol [Param] Type Stmt
    | Typedef     TextPos Symbol AnnoType
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Expr (Maybe Pattern) Stmt
    | Data        TextPos Symbol Type (Maybe Expr)
    deriving (Eq, Show)


data AnnoADTField
    = ADTFieldMember Symbol [Type]
    | ADTFieldType   Type
    | ADTFieldNull
    deriving (Eq)


data AnnoType
    = AnnoType  Type
    | AnnoTuple [(Symbol, Type)]
    | AnnoADT   [AnnoADTField]
    | AnnoEnum  [Symbol]
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
        Initialiser  p _ -> p
        Field        p _ _ -> p
        Subscript    p _ _ -> p
        Ident        p _ -> p
        Call         p _ _ _ -> p 
        Builtin      p _ _ _ -> p 
        Conv         p _ _ -> p
        Prefix       p _ _ -> p
        Infix        p _ _ _ -> p
        ADT          p _ -> p
        Match        p _ _ -> p
        Range        p _ _ _ -> p


instance TextPosition Stmt where
    textPos stmt = case stmt of
        Assign      p _ _ -> p
        Set         p _ _ -> p
        Print       p _ -> p
        ExprStmt    e -> textPos e
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncDef     p _ _ _ _ _ -> p
        Typedef     p _ _ -> p
        Switch      p _ _ -> p
        For         p _ _ _ -> p
        Data        p _ _ _ -> p

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


instance Show Operator where
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
        AnnoEnum ss  -> brcStrs $ map show ss

convertAnno :: AnnoType -> AnnoType
convertAnno anno = case anno of
    AnnoADT xs | all isEnumField xs -> AnnoEnum $ map (\(ADTFieldMember s []) -> s) xs
    _ -> anno
    where
        isEnumField :: AnnoADTField -> Bool
        isEnumField (ADTFieldMember _ []) = True
        isEnumField _                     = False


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
        Initialiser pos exprs         -> arrStrs (map show exprs)
        Field pos expr symbol         -> show expr ++ "." ++ show symbol
        Subscript pos expr1 expr2     -> show expr1 ++ "[" ++ show expr2 ++ "]"
        Ident p s                     -> show s 
        Conv pos typ exprs            -> show typ ++ tupStrs (map show exprs)
        Prefix pos op expr            -> show op ++ show expr
        Infix pos op expr1 expr2      -> show expr1 ++ " " ++ show op ++ " " ++ show expr2
        ADT pos expr                  -> brcStrs [show expr]
        Call pos [] symbol exprs      -> show symbol ++ tupStrs (map show exprs)
        Call pos params symbol exprs  -> brcStrs (map show params) ++ "." ++ show symbol ++ tupStrs (map show exprs)
        Builtin pos params sym exprs  -> brcStrs (map show params) ++ "." ++ sym ++ tupStrs (map show exprs)
        Match pos expr1 expr2         -> show expr1 ++ " -> " ++ show expr2
        Range pos mexpr mexpr1 mexpr2 -> maybe "" show mexpr ++ "[" ++ maybe "" show mexpr1 ++ ".." ++ maybe "" show mexpr2 ++ "]"


-- every function must end on a newline and print pre before every line
prettyAST :: AST -> IO ()
prettyAST ast = do
    putStrLn $ "module " ++ (astModuleName ast)

    putStrLn ""

    forM_ (astImports ast) $ \imp ->
        putStrLn $ show imp

    putStrLn ""

    mapM_ (prettyStmt "") (astStmts ast)
    where
        prettyStmt :: String -> Stmt -> IO ()
        prettyStmt pre stmt = case stmt of
            FuncDef pos params symbol args retty blk -> do
                paramStr <- case params of
                    [] -> return ""
                    ps -> return $ tupStrs $ map show ps
                
                putStrLn $ pre ++ "fn " ++ paramStr ++ show symbol ++ tupStrs (map show args) ++ " " ++ if retty == Void then "" else show retty
                prettyStmt (pre ++ "\t") blk
                putStrLn ""

            Assign pos pat expr        -> putStrLn $ pre ++ "let " ++ show pat ++ " = " ++ show expr
            Set pos ind expr           -> putStrLn $ pre ++ show ind ++ " = " ++ show expr
            Print pos exprs            -> putStrLn $ pre ++ "print" ++ tupStrs (map show exprs)
            Return pos mexpr -> putStrLn $ pre ++ "return " ++ maybe "" show mexpr
 
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

            Typedef pos symbol anno -> do
                putStrLn $ pre ++ "typedef " ++ show symbol ++ " " ++ show anno

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

            _  -> error $ "invalid stmt: " ++ show stmt

