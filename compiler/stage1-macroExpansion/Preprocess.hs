{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Preprocess where

import Control.Monad.Identity
import Control.Monad.State

import AST
import Symbol
import InstBuilder
import AstBuilder
import Error
import Type


newtype PreprocessInst a = PreprocessInst
    { unPreprocessInst :: State InstBuilderState a }
    deriving (Functor, Applicative, Monad, MonadState InstBuilderState)


instance MonadInstBuilder PreprocessInst where
    liftInstBuilderState (StateT s) = PreprocessInst (StateT s)


runPreprocessInst :: InstBuilderState -> PreprocessInst a -> (a, InstBuilderState)
runPreprocessInst instBuilderState f
    = runState (unPreprocessInst f) instBuilderState


newtype PreprocessAst a = PreprocessAst
    { unPreprocessAst :: State AstBuilderState a }
    deriving (Functor, Applicative, Monad, MonadState AstBuilderState)


instance MonadAstBuilder PreprocessAst where
    liftAstBuilderState (StateT s) = PreprocessAst (StateT s)


runPreprocessAst :: AstBuilderState -> PreprocessAst a -> (a, AstBuilderState)
runPreprocessAst astBuilderState f
    = runState (unPreprocessAst f) astBuilderState



preprocess :: AST -> AstBuilderState
preprocess ast = do
    snd $ runPreprocessAst (initAstBuilderState (astModuleName ast) (astImports ast)) preprocess'
    where
        preprocess' :: PreprocessAst ()
        preprocess' = forM_ (astStmts ast) $ \stmt -> case stmt of
            Function _ _ _ _ _ -> addTopStmt (TopStmt stmt)
            Derives _ _ _ _     -> addTopStmt (TopStmt stmt)
            Typedef _ _ _ _     -> addTopStmt (TopStmt stmt)
            x                   -> buildTopStatement x


buildInst :: PreprocessInst a -> PreprocessAst InstBuilderState
buildInst f = do
    return $ snd $ runPreprocessInst initInstBuilderState (withCurId 0 $ void f)


buildTopStatement :: Stmt -> PreprocessAst ()
buildTopStatement statement = case statement of
    MacroTuple pos generics symbol fields -> do
        void $ addTopStmt $ TopStmt $ Typedef pos generics symbol (foldl Apply Tuple $ map snd fields)
        forM_ (zip fields [0..]) $ \((fieldSymbol, fieldType), i) -> do
            -- write field feature
            let a = Sym ["A"]
            let b = Sym ["B"]
            addTopStmt $ TopStmt $ Function pos [a, b] [(a, b)] fieldSymbol $ foldType [Type.Func, TypeDef b, TypeDef a]

            -- write instance
            let typ = foldType (TypeDef symbol : map TypeDef generics)
            let acq = foldType [TypeDef fieldSymbol, typ, fieldType]
            addTopStmt . TopInst pos generics acq [Param pos (Sym ["x"]) (Apply Type.Ref Tuple)] True =<<
                buildInst (appendStmt $ Return pos . Just $ field fieldType i (Ident pos $ Sym ["x"]))

            let acq2 = foldType [TypeDef fieldSymbol, Apply Table typ, Apply Slice fieldType]
            addTopStmt . TopInst pos generics acq2 [Param pos (Sym ["x"]) (Apply Ref Tuple)] False =<<
                buildInst (appendStmt $ Return pos . Just $ field (Apply Slice fieldType) i (Ident pos $ Sym ["x"]))

    Instance pos generics typ args isRef (Block stmts) -> do
        addTopStmt . TopInst pos generics typ args isRef =<< buildInst (mapM_ buildStmt stmts)

    FuncInst pos generics symbol params retty (Block stmts) -> do
        argTypes <- forM params $ \param -> case param of
            Param pos symbol (Apply Type.Ref t) -> return t
            Param pos symbol t                  -> return t

        isRef <- case retty of
            RefRetty _ -> return True
            Retty _    -> return False

        addTopStmt $ TopStmt $ Function pos generics [] symbol $ foldType (Type.Func : typeof retty : argTypes)
        inst' <- buildInst (mapM_ buildStmt stmts)
        addTopStmt $ TopInst pos generics (foldl Apply (TypeDef symbol) $ map TypeDef generics) params isRef inst'


    Enum pos generics symbol cases -> do
        addTopStmt $ TopStmt $ Enum pos generics symbol cases
    x -> error (show x)


field :: Type -> Integer -> Expr -> Expr
field fieldType i expr = AExpr fieldType $ Call (textPos expr) typ [expr]
    where
        typ = foldType [TypeDef (Sym ["builtin", "builtinField"]), Size (fromIntegral i), Type 0, Type 0]
    

buildBlock :: Stmt -> PreprocessInst ID
buildBlock (Block stmts) = do
    id <- newStmt (Block [])
    withCurId id $ mapM buildStmt stmts
    return id
buildBlock stmt = do
    id <- newStmt (Block [])
    withCurId id $ buildStmt stmt
    return id


buildStmt :: Stmt -> PreprocessInst ()
buildStmt statement = case statement of
    ExprStmt expr     -> void $ appendStmt (ExprStmt expr)
    EmbedC _ _ _      -> void $ appendStmt statement
    Return pos mexpr  -> void $ appendStmt statement

    Block stmts -> do
        id <- appendStmt (Block [])
        withCurId id (mapM_ buildStmt stmts)

    Let pos pat mexpr Nothing -> do
        void $ appendStmt (Let pos pat mexpr Nothing)

    If pos expr blk mblk -> do
        trueId <- buildBlock blk
        falseId <- traverse buildBlock mblk
        void $ appendStmt $ If pos expr (Stmt trueId) (fmap Stmt falseId)

    Switch pos expr cases -> do
        cases' <- forM cases $ \(pat, Block stmts) -> do
            id <- newStmt (Block [])
            withCurId id (mapM_ buildStmt stmts)
            return (pat, Stmt id)
        void $ appendStmt (Switch pos expr cases')

    For pos expr mpat (Block stmts) -> do
        id <- newStmt (Block [])
        withCurId id (mapM_ buildStmt stmts)
        void $ appendStmt $ For pos expr mpat (Stmt id)

    While pos expr (Block stmts) -> do
        id <- newStmt (Block [])
        withCurId id (mapM buildStmt stmts)
        void $ appendStmt $ While pos expr (Stmt id)

    With pos exprs (Block stmts) -> do
        id <- newStmt (Block [])
        withCurId id (mapM buildStmt stmts)
        void $ appendStmt $ With pos exprs (Stmt id)

    x -> error (show x)
