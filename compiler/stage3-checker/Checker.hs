module Checker where

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
    = Object { objChildren :: [Symbol] }


data CheckState
    = CheckState
        { symTab :: SymTab.SymTab Symbol () Object
        }


initCheckState = CheckState
    { symTab = SymTab.initSymTab
    }


runASTChecker :: ASTResolved -> DoM s ()
runASTChecker ast = fmap fst $ runDoMExcept initCheckState (checkAST ast)


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
    


assertSymbolResolved :: Symbol -> DoM CheckState ()
assertSymbolResolved (SymResolved _ _ _) = return ()
assertSymbolResolved symbol              = fail $ "unresolved symbol: " ++ show symbol

assertTypeNoReferences :: Type -> DoM CheckState ()
assertTypeNoReferences typ = case typ of
    Type.Tuple t -> assertRecordNoReferences t

    x -> error (show x)
    where
        assertRecordNoReferences :: Type -> DoM CheckState ()
        assertRecordNoReferences typ = case typ of
            x -> error (show x)




checkAST :: ASTResolved -> DoM CheckState ()
checkAST ast = do
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> do
        when (funcTypeArgs body == []) $
            checkFuncDef symbol body


checkFuncDef :: Symbol -> FuncBody -> DoM CheckState ()
checkFuncDef symbol body = do
    checkStmt (funcStmt body)
    return ()


checkStmt :: Stmt -> DoM CheckState ()
checkStmt stmt = case stmt of
    Block stmts -> do
        pushSymTab
        mapM_ checkStmt stmts
        popSymTab

    If _ cnd blk mblk -> do
        checkStmt blk
        void $ traverse checkStmt mblk

    Return _ mexpr -> do
        return ()

    ExprStmt expr -> do
        return ()

    EmbedC _ _ -> return ()

    Data _ symbol typ Nothing -> do
        assertSymbolResolved symbol
        --assertTypeNoReferences typ
        define symbol (Object [])
        return ()

    SetOp _ op expr1 expr2 -> return ()

    Increment _ expr -> return ()

    Let _ pat expr mblk -> do
        checkPattern [] pat 
        void $ traverse checkStmt mblk

    Switch _ expr cases -> do
        forM_ cases $ \(pat, blk) -> do
            checkStmt blk

    While _ cnd blk -> do
        checkStmt blk

    x -> error (show x)


checkPattern :: [Symbol] -> Pattern -> DoM CheckState ()
checkPattern parents (PatAnnotated pattern patType) = case pattern of
    PatIdent _ symbol -> do
        assertSymbolResolved symbol

    PatRecord _ pats -> do
        forM_ pats $ \pat -> case pat of
            PatAnnotated (PatIdent _ s) t -> do
                return ()

            _ -> return ()

        mapM_ (checkPattern parents) pats

    PatField _ symbol pats -> do
        assertSymbolResolved symbol
        mapM_ (checkPattern parents) pats

    PatLiteral expr -> do
        return ()

    PatTuple _ pats -> do
        mapM_ (checkPattern parents) pats

    x -> error (show x)
checkPattern _ pattern = withPos pattern $ fail "unresolved pattern"
