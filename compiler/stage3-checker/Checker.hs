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
    = ObjUses Symbol
    | ObjDefines Symbol [Symbol]
    deriving (Show, Eq, Ord)


data CheckState
    = CheckState
        { symTab      :: SymTab.SymTab Symbol () Object
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


define :: Symbol -> Object -> DoM CheckState ()
define symbol object = do
    isDefined <- isJust . SymTab.lookup symbol () <$> gets symTab
    check (not isDefined) (show symbol ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert symbol () object (symTab s) }


look :: Symbol -> DoM CheckState Object
look symbol = do
    isDefined <- isJust . SymTab.lookup symbol () <$> gets symTab
    check (isDefined) (show symbol ++ " not defined")
    fmap fromJust $ SymTab.lookup symbol () <$> gets symTab


checkAST :: ASTResolved -> DoM CheckState ()
checkAST ast = do
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            pushSymTab
            objects <- checkStmt (funcStmt body)
            popSymTab


checkUses :: [Object] -> DoM CheckState ()
checkUses objs = do
    let uses = filter isUse objs
    let dups = checkDup uses
    case dups of
        [] -> return ()
        (x:xs) -> fail ("multiple uses: " ++ show x)
    where
        isUse :: Object -> Bool
        isUse (ObjUses _) = True
        isUse _           = False

        checkDup :: Eq a => [a] -> [a]
        checkDup (x:xs) = if x `elem` xs then x : (checkDup xs) else checkDup xs
        checkDup []     = []


pack :: [Object] -> [Object]
pack = Set.toList . Set.fromList


checkExpr :: Expr -> DoM CheckState [Object]
checkExpr (AExpr exprType expression) = withPos expression $ case expression of
    Infix pos op expr1 expr2 -> do
        objs1 <- checkExpr expr1
        objs2 <- checkExpr expr2
        checkUses (objs1 ++ objs2)
        return []

    Prefix pos op expr -> do
        return []

    Field pos expr ident -> checkExpr expr

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

    RecordAccess pos expr -> checkExpr expr

    Ident pos symbol -> return [ObjUses symbol]

    Match pos pat expr -> return []

    Builtin pos symbol exprs -> return []

    Int pos n -> return []

    AST.Bool pos b -> return []

    AST.String pos s -> return []

    AST.Record pos exprs -> return []

    AST.Tuple pos exprs -> return []

    RecordAccess pos expr -> return []

    Subscript pos expr1 expr2 -> return []

    x -> fail (show x)
    


checkStmt :: Stmt -> DoM CheckState [Object]
checkStmt stmt = case stmt of
    Block stmts -> do
        pushSymTab
        mapM_ checkStmt stmts
        popSymTab
        return []

    If _ cnd blk mblk -> do
        objs <- checkExpr cnd
        checkStmt blk
        void $ traverse checkStmt mblk
        return []

    Return _ mexpr -> do
        return []

    ExprStmt expr -> do
        return []

    EmbedC _ _ -> do
        return []

    Data _ symbol typ Nothing -> do
        --assertTypeNoReferences typ
        define symbol (ObjDefines symbol [])
        return []

    SetOp _ op expr1 expr2 -> do
        return []

    Increment _ expr -> do
        return []

    Let _ pat mexpr mblk -> do
        checkPattern [] pat 
        void $ traverse checkExpr mexpr
        void $ traverse checkStmt mblk
        return []

    Switch _ expr cases -> do
        forM_ cases $ \(pat, blk) -> do
            checkStmt blk
        return []

    While _ cnd blk -> do
        checkStmt blk
        return []

    x -> error (show x)


checkPattern :: [Symbol] -> Pattern -> DoM CheckState ()
checkPattern parents (PatAnnotated pattern patType) = case pattern of
    PatIdent _ symbol -> do
        return ()

    PatRecord _ pats -> do
        forM_ pats $ \pat -> case pat of
            PatAnnotated (PatIdent _ s) t -> do
                return ()

            _ -> return ()

        mapM_ (checkPattern parents) pats

    PatField _ symbol pats -> do
        mapM_ (checkPattern parents) pats

    PatLiteral expr -> do
        return ()

    PatTuple _ pats -> do
        mapM_ (checkPattern parents) pats
    PatIgnore _ -> return ()

    x -> error (show x)
checkPattern _ pattern = withPos pattern $ fail "unresolved pattern"
