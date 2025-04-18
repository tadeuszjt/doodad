module AST where

import Prelude hiding (LT, GT)
import Data.Maybe
import Data.List
import Control.Monad
import Type 
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
    = Import Bool Bool FilePath (Maybe String)
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

type Generics = [Symbol]
type FunDeps  = [(Symbol, Symbol)]

data Retty
    = Retty Type
    | RefRetty Type
    deriving (Eq, Ord)

instance TextPosition Param where
    textPos = paramPos

instance Type.Typeof Param where
    typeof = paramType

instance Type.Typeof Retty where
    typeof (Retty t) = t
    typeof (RefRetty t) = t

data Pattern
    = Pattern      Int
    | PatLiteral   Expr                   -- equal(a, b)
    | PatIgnore    TextPos                -- fine
    | PatIdent     TextPos Symbol         -- set(&ident, b)
    | PatTuple     TextPos [Pattern]      -- set(&a, first(&z)); set(&b, second(&z)) ...
    | PatSlice     TextPos [Pattern]      
    | PatGuarded   TextPos Pattern Expr   -- fine
    | PatAnnotated Pattern Type           -- fine
    | PatField     TextPos Symbol Pattern -- just(y) | x.isJust
    deriving (Eq)

data Expr
    = AExpr        Type  Expr
    | Int          TextPos Integer
    | Float        TextPos Double
    | Bool         TextPos Bool
    | Char         TextPos Char
    | String       TextPos String
    | Call         TextPos Type [Expr]
    | Ident        TextPos Symbol
    | Match        TextPos Expr Pattern
    | Reference    TextPos Expr
    | Array        TextPos [Expr]
    | Expr         Int
    deriving (Eq)

instance Typeof Expr where
    typeof (AExpr typ _) = typ
    typeof a = error $ "can only take typeof AExpr: " ++ show a

instance Typeof Pattern where
    typeof (PatAnnotated _ typ) = typ
    typeof a = error $ "can only take typeof PatAnnotated" 


data Stmt
    = Stmt        Int
    | Let         TextPos Pattern (Maybe Expr) (Maybe Stmt)
    | With        TextPos [Expr] Stmt
    | ExprStmt    Expr
    | Return      TextPos (Maybe Expr)
    | Block       [Stmt]
    | If          TextPos Expr Stmt (Maybe Stmt)
    | While       TextPos Expr Stmt
    | FuncInst    TextPos Generics Symbol [Param] Retty Stmt
    | Function    TextPos Generics FunDeps Symbol Type
    | Instance    TextPos Generics Type [Param] Bool Stmt
    | Typedef     TextPos Generics Symbol Type
    | Switch      TextPos Expr [(Pattern, Stmt)]
    | For         TextPos Expr (Maybe Pattern) Stmt
    | EmbedC      TextPos [(String, Symbol)] String
    | Enum        TextPos Generics Symbol [ (Symbol, [Type] ) ]
    | MacroTuple  TextPos Generics Symbol [ (Symbol, Type) ]
    | Derives     TextPos Generics Type [Type]
    deriving (Eq, Show)


instance TextPosition Pattern where
    textPos pattern = case pattern of
        PatLiteral   e -> textPos e
        PatIgnore    p -> p
        PatIdent     p _ -> p
        PatTuple     p _ -> p
        PatGuarded   p _ _ -> p
        PatSlice     p _ -> p
        PatAnnotated pat _ -> textPos pat
        PatField     p _ _ -> p


instance TextPosition Expr where
    textPos expression = case expression of
        AExpr        t expr -> textPos expr
        Int          p _    -> p
        Float        p _    -> p
        AST.Bool     p _    -> p
        AST.Char    p _    -> p
        String       p _    -> p
        Ident        p _    -> p
        Call         p _ _ -> p 
        Match        p _ _ -> p
        Reference    p _ -> p
        AST.Array    p _ -> p
        _ -> error (show expression)


instance TextPosition Stmt where
    textPos stmt = case stmt of
        ExprStmt    expr -> textPos expr
        Let         p _ _ _ -> p
        Return      p _ -> p
        Block       s -> textPos (head s)
        If          p _ _ _ -> p
        While       p _ _ -> p
        FuncInst     p _ _ _ _ _ -> p
        Typedef     p _ _ _ -> p
        Switch      p _ _ -> p
        For         p _ _ _ -> p
        EmbedC      p _ _ -> p
        Function     p _ _ _ _ -> p
        Enum        p _ _ _ -> p
        MacroTuple  p _ _ _ -> p
        Instance     p _ _ _ _ _ -> p
        Derives     p _ _ _ -> p
        x -> error ("invalid statement")

tupStrs, arrStrs, brcStrs :: [String] -> String
tupStrs strs = "(" ++ intercalate ", " strs ++ ")"
arrStrs strs = "[" ++ intercalate ", " strs ++ "]"
brcStrs strs = "{" ++ intercalate ", " strs ++ "}"


instance Show Import where
    show (Import isExport isQualified path mname) =
        (if isExport then "export " else "import ")
        ++ (if isQualified then "qualified " else "")
        ++ path
        ++ maybe "" id mname
    show (CInclude path) = "#include " ++ path
    show (CLink path) = "link " ++ path

instance Show Param where
    show (Param pos name typ)    = prettySymbol name ++ " " ++ show typ
    show (RefParam pos name typ) = prettySymbol name ++ "& " ++ show typ


instance Show Retty where
    show (Retty t) = show t
    show (RefRetty t) = "&" ++ show t


instance Show Pattern where
    show pattern = case pattern of
        Pattern i                -> show i
        PatLiteral c             -> show c
        PatIgnore pos            -> "_"
        PatIdent pos symbol      -> prettySymbol symbol
        PatTuple pos ps          -> tupStrs (map show ps)
        PatGuarded pos pat expr  -> show pat ++ " | " ++ show expr
        PatAnnotated pat typ     -> show pat ++ ":" ++ show typ
        PatField _ symbol pat    -> prettySymbol symbol ++ " " ++ show pat
        PatSlice _ pats          -> arrStrs (map show pats)


instance Show Expr where
    show expression = case expression of
        AExpr t expr                       -> show expr ++ ":" ++ show t 
        Expr i                             -> show i
        Int pos n                          -> show n
        Float pos f                        -> show f
        AST.Bool pos b                     -> if b then "true" else "false"
        AST.Char pos c                     -> show c
        String pos s                       -> show s
        AST.Array pos exprs                -> arrStrs (map show exprs)
        Ident p s                          -> prettySymbol s 
        Call pos typ exprs                  -> show typ ++ tupStrs (map show exprs)
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


prettyStmt :: String -> Stmt -> IO ()
prettyStmt pre stmt = case stmt of
    Return pos mexpr -> putStrLn $ pre ++ "return " ++ maybe "" show mexpr
    ExprStmt callExpr -> putStrLn $ pre ++ show callExpr
    Block stmts -> mapM_ (prettyStmt (pre ++ "\t")) stmts
    EmbedC pos m str -> putStrLn $ pre ++ "$" ++ str

    FuncInst _ generics funcSymbol funcArgs funcRetty blk -> do
        putStrLn ""
        putStrLn $
            pre
            ++ "fn"
            ++ genericsStr generics
            ++ " "
            ++ prettySymbol funcSymbol
            ++ tupStrs (map show funcArgs)
            ++ " "
            ++ show funcRetty
        prettyStmt pre blk

    Function pos generics funDeps symbol funcType  -> do
        let funDepsStr = case funDeps of
                [] -> ""
                xs -> " | " ++ intercalate ", " (map (\(a, b) -> show a ++ "->" ++ show b) funDeps)
        let genericsStr = case generics of
                [] -> "" 
                xs -> brcStrs [intercalate ", " (map prettySymbol xs), funDepsStr]
        putStrLn ""
        putStrLn $ pre ++ "func" ++ genericsStr ++ " " ++ prettySymbol symbol ++ " " ++ show funcType

    Let pos pat mexpr mblk -> do
        exprStr <- case mexpr of
            Just expr -> return $ " = " ++ show expr
            Nothing   -> return $ ""
        putStrLn $ pre ++ "let " ++ show pat ++ exprStr ++ if isJust mblk then " in" else ""
        when (isJust mblk) $ prettyStmt pre (fromJust mblk)

    If pos cnd true mfalse -> do
        putStrLn $ pre ++ "if " ++ show cnd
        prettyStmt pre true
        case mfalse of
            Nothing -> return ()
            Just false -> do 
                putStrLn (pre ++ "else")
                prettyStmt pre false


    While pos cnd stmt -> do
        putStrLn (pre ++ "while " ++ show cnd)
        prettyStmt pre stmt

    Typedef pos generics symbol anno -> do
        putStrLn $ pre ++ "type" ++ genericsStr generics ++ " " ++ prettySymbol symbol ++ " " ++ show anno

    Instance pos generics typ args isRef stmt -> do
        putStrLn ""
        putStrLn $ pre
            ++ "instance"
            ++ genericsStr generics
            ++ " "
            ++ show typ
            ++ " "
            ++ tupStrs (map show args)
            ++ (case isRef of False -> ""; True -> " -> &")
        prettyStmt "" stmt
        

    Switch pos expr cases -> do
        putStrLn $ pre ++ "switch " ++ show expr
        forM_ cases $ \(pat, stmt) -> do
            putStrLn $ pre ++ "\t" ++ show pat
            prettyStmt (pre ++ "\t") stmt

    For pos expr mcnd blk -> do
        let cndStr = maybe "" ((" -> " ++) . show) mcnd
        let exprStr = "" ++ show expr
        putStrLn $ pre ++ "for " ++ exprStr ++ cndStr
        prettyStmt pre blk

    Derives pos generics t1 ts -> do
        putStrLn $ pre ++ "derives" ++ genericsStr generics ++ " " ++ show t1 ++ " " ++ tupStrs (map show ts)

    MacroTuple pos generics symbol fields -> do
        putStrLn $ pre ++ "tuple" ++ genericsStr generics ++ " " ++ prettySymbol symbol ++ " {"

        forM_ fields $ \(fieldSymbol, fieldType) -> do
            putStrLn $ pre ++ "\t" ++ prettySymbol fieldSymbol ++ " " ++ show fieldType

        putStrLn "}"

    Enum pos generics symbol fields -> do
        putStrLn $ pre ++ "enum" ++ genericsStr generics ++ " " ++ prettySymbol symbol ++ " {"

        forM_ fields $ \(fieldSymbol, fieldTypes) -> do
            putStrLn $ pre ++ "\t" ++ prettySymbol fieldSymbol ++ tupStrs (map show fieldTypes)
        
        putStrLn "}"

    x  -> error (show x)

    where
        genericsStr :: [Symbol] -> String
        genericsStr [] = ""
        genericsStr xs = "{" ++ intercalate ", " (map prettySymbol xs) ++ "}"

