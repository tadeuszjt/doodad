{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe

import AST as S
import Type as T
import Monad
import Error
import Control.Monad.State
import qualified SymTab


type SymTab = SymTab.SymTab String SymKey Object

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc [Type]
    | KeyMember Type
    deriving (Show, Eq, Ord)

data Object
    = ObjVar Type
    | ObjType Type
    | ObjFunc Type
    | ObjMember Int
    deriving (Show, Eq)


define :: BoM SymTab m => String -> SymKey -> Object -> m ()
define sym key obj = do
    resm <- SymTab.lookupHead sym key <$> get
    when (isJust resm) $ fail $ sym ++ " already defined"
    modify $ SymTab.insert sym key obj


collectAST :: BoM SymTab m => AST -> m [(Type, Type)]
collectAST ast = do
    forM [ stmt | stmt@(S.Typedef _ _ _) <- astStmts ast ] $
        \(S.Typedef pos sym annoTyp) ->
            withPos pos $ define sym KeyType =<< case annoTyp of
                AnnoType t   -> return (ObjType t)

                AnnoTuple xs   -> do
                    let typedef = T.Typedef (Sym sym)
                    forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                    return $ ObjType $ T.Tuple (map snd xs)

                AnnoADT xs -> do
                    let typedef = T.Typedef (Sym sym)
                    forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                    return $ ObjType $ T.ADT (map snd xs)

    forM [ stmt | stmt@(S.FuncDef _ _ _ _ _) <- astStmts ast ] $
        \(S.FuncDef pos sym params retty _) ->
            define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    fmap concat $ mapM collectStmt (astStmts ast)


collectStmt :: BoM SymTab m => Stmt -> m [(Type, Type)]
collectStmt stmt = withPos stmt $ case stmt of
    FuncDef _ sym params retty blk -> do
        return []


    _ -> return []


collectExpr :: BoM SymTab m => Expr -> m [(Type, Type)]
collectExpr expr = case expr of
    _ -> fail "3213)21"
