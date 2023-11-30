{-# LANGUAGE FlexibleInstances #-}
module Checker where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.Maybe

import AST
import Type
import ASTResolved
import Monad
import Symbol
import qualified SymTab
import Error


data Object
    = Referencing Symbol -- presenting a reference to
    | Defining Symbol    -- defining   a reference to
    deriving (Show, Eq, Ord)


isReferencing :: Object -> Bool
isReferencing (Referencing _) = True
isReferencing _               = False

isDefining :: Object -> Bool
isDefining (Defining _) = True
isDefining _            = False

data CheckState
    = CheckState
        { symTab      :: SymTab.SymTab Object () ()
        , astResolved :: ASTResolved
        }


initCheckState ast = CheckState
    { symTab      = SymTab.initSymTab
    , astResolved = ast
    }

instance TypeDefs (DoM CheckState) where getTypeDefs = gets (typeFuncs . astResolved)


runASTChecker :: ASTResolved -> DoM s ()
runASTChecker ast = fmap fst $ runDoMExcept (initCheckState ast) (checkAST ast)


pushSymTab :: DoM CheckState ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: DoM CheckState ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


define :: Object -> DoM CheckState ()
define object = do
    isDefined <- isJust . SymTab.lookup object () <$> gets symTab
    check (not isDefined) (show object ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert object () () (symTab s) }


look :: Object -> DoM CheckState Bool
look object = do
    isJust . SymTab.lookup object () <$> gets symTab


checkAST :: ASTResolved -> DoM CheckState ()
checkAST ast = do
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            pushSymTab
            objects <- checkStmt (funcStmt body)
            popSymTab


checkMultipleReferences :: [Object] -> DoM CheckState ()
checkMultipleReferences objs = do
    let uses = filter isReferencing objs
    let dups = checkDup uses
    case dups of
        [] -> return ()
        (x:xs) -> fail ("multiple uses: " ++ show x)
    where
        checkDup :: Eq a => [a] -> [a]
        checkDup (x:xs) = if x `elem` xs then x : (checkDup xs) else checkDup xs
        checkDup []     = []


pack :: [Object] -> [Object]
pack = Set.toList . Set.fromList


checkExpr :: Expr -> DoM CheckState [Object]
checkExpr (AExpr exprType expression) = withPos expression $ case expression of
    Int pos n -> return []
    AST.Bool pos b -> return []
    AST.String pos s -> return []

    AST.Tuple pos exprs -> do
        mapM_ checkExpr exprs
        return []

    -- returns the references of the expressions
    AST.Record pos exprs -> do
        objs <- fmap concat $ mapM checkExpr exprs
        checkMultipleReferences objs
        return objs

    -- returns the references of the expr
    Field pos expr ident -> checkExpr expr

    -- returns one reference
    Ident pos symbol -> do
        b <- look (Referencing symbol)
        when b $ fail $ "invalid reference: " ++ (Symbol.sym symbol)
        return [Referencing symbol]

    -- returns the references of the expr
    RecordAccess pos expr -> do
        objs <- checkExpr expr
        return objs

    -- ensures no multiple references active
    Infix pos op expr1 expr2 -> do
        objs1 <- checkExpr expr1
        objs2 <- checkExpr expr2
        checkMultipleReferences (objs1 ++ objs2)
        return []

    -- return objects from mparam if returns record
    Call pos mparam symbol exprs -> do
        void $ mapM checkExpr exprs
        resm <- traverse checkExpr mparam
        objs <- case resm of
            Nothing -> return []
            Just res -> return res
        base <- baseTypeOf exprType
        case base of
            Type.Record _ -> return objs
            _             -> return []

    -- pat defines, redefines references of expr.
    Match pos expr pat -> do
        objs1 <- checkExpr expr
        ss <- checkPattern pat
        case ss of
            [] -> return []
            _  -> return [ Referencing s | Referencing s <- objs1 ] 

    Builtin pos symbol exprs -> return []

    Construct pos typ exprs -> return []

    Prefix pos op expr -> do
        return []


    x -> fail (show x)
    


checkStmt :: Stmt -> DoM CheckState ()
checkStmt stmt = withPos stmt $ case stmt of
    Block stmts -> do
        pushSymTab
        mapM_ checkStmt stmts
        popSymTab

    If _ cnd blk mblk -> do
        pushSymTab
        objs <- checkExpr cnd
        mapM define [ Referencing s | Referencing s <- objs ]
        checkStmt blk
        traverse checkStmt mblk
        popSymTab

    Return _ mexpr -> do
        void $ traverse checkExpr mexpr

    ExprStmt expr -> do
        checkExpr expr
        return ()

    EmbedC _ _ -> do
        return ()

    Data _ symbol typ Nothing -> do
        return ()

    SetOp _ op expr1 expr2 -> do
        objs1 <- checkExpr expr1
        objs2 <- checkExpr expr2
        checkMultipleReferences (objs1 ++ objs2)
        return ()

    Let _ pat mexpr mblk -> do
        pushSymTab
        mobjs <- traverse checkExpr mexpr
        objs <- case mobjs of
            Nothing -> return []
            Just xs -> return xs

        ss <- checkPattern pat 
        case ss of
            [] -> return ()
            ss -> mapM_ define [ Referencing s | Referencing s <- objs ]
        traverse checkStmt mblk
        popSymTab

    Switch _ expr cases -> do
        objs <- checkExpr expr
        forM_ cases $ \(pat, blk) -> do
            pushSymTab
            ss <- checkPattern pat
            case ss of
                [] -> return ()
                ss -> mapM_ define [ Referencing s | Referencing s <- objs ]

            checkStmt blk
            popSymTab
        return ()

    While _ cnd blk -> do
        checkExpr cnd
        checkStmt blk
        return ()

    x -> error (show x)


-- The only way that patterns concern references is by defining them in the record pattern.
-- Return all defined symbols to references
checkPattern :: Pattern -> DoM CheckState [Symbol]
checkPattern (PatAnnotated pattern patType) = withPos pattern $ case pattern of
    PatIdent _ symbol -> return []
    PatIgnore _       -> return []
    PatLiteral expr   -> checkExpr expr >> return []

    PatGuarded _ pat expr -> do
        checkExpr expr
        checkPattern pat

    PatTuple _ pats -> fmap concat $ mapM checkPattern pats

    PatField _ symbol pats -> fmap concat $ mapM checkPattern pats

    PatRecord _ pats -> do
        ss <- fmap concat $ mapM checkPattern pats
        return $ [ s | PatAnnotated (PatIdent _ s) _ <- pats ] ++ ss

    x -> error (show x)
checkPattern _ = return []
checkPattern pattern = withPos pattern $ fail $ "unresolved pattern: " ++ show pattern
