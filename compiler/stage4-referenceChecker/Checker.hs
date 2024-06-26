{-# LANGUAGE FlexibleInstances #-}
module Checker where

import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List

import AST
import Type
import ASTResolved
import Monad
import Symbol
import qualified SymTab
import Error


data Node
    = NodeNull
    | NodeArg Symbol
    | NodeData Symbol
    | NodeDefine Symbol
    | NodeUnion [Node]
    deriving (Eq, Ord)



isBaseNode :: Node -> Bool
isBaseNode node = case node of
    NodeArg _ -> True
    NodeUnion nodes -> False
    NodeData _ -> True
    NodeDefine _ -> True
    x -> error "invalid node"


listNodes :: Node -> [Node]
listNodes node = case node of
    NodeArg symbol -> [node]
    NodeDefine _ -> [node]
    NodeData _ -> [node]
    NodeUnion nodes -> (node : concat (map listNodes nodes))
    NodeNull -> []
    x -> error "invalid node"


checkNoSameBaseNodes :: MonadFail m => [Node] -> m ()
checkNoSameBaseNodes nodes = do
    let bases = filter isBaseNode $ concat (map listNodes nodes)
    unless (length bases == length (nub bases)) (fail "error, multiple refernences to same data")



data CheckState
    = CheckState
        { symTab      :: SymTab.SymTab Symbol Node
        , astResolved :: ASTResolved
        }


initCheckState ast = CheckState
    { symTab      = SymTab.initSymTab
    , astResolved = ast
    }

instance TypeDefs (DoM CheckState) where getTypeDefs = gets (typeDefsAll . astResolved)


runASTChecker :: ASTResolved -> DoM s ()
runASTChecker ast = fmap fst $ runDoMExcept (initCheckState ast) (checkAST ast)


pushSymTab :: DoM CheckState ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }
--
--
popSymTab :: DoM CheckState ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


define :: Symbol -> Node -> DoM CheckState ()
define symbol node = do
    --liftIO $ putStrLn $ "defining: " ++ show symbol
    isDefined <- isJust . SymTab.lookup symbol <$> gets symTab
    check (not isDefined) (prettySymbol symbol ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert symbol node (symTab s) }


look :: Symbol -> DoM CheckState Node
look symbol = do
    --liftIO $ putStrLn $ "looking: " ++ show symbol
    resm <- SymTab.lookup symbol <$> gets symTab
    case resm of
        Nothing -> fail $ prettySymbol symbol ++ " not defined"
        Just res -> return res


checkAST :: ASTResolved -> DoM CheckState ()
checkAST ast = do
    error "here"
--    forM_ (funcInstance ast) $ \func -> do
--        pushSymTab
--        forM (funcArgs (funcHeader func)) $ \param -> do
--            define (paramSymbol param) (NodeArg $ paramSymbol param)
--
--        checkStmt (funcStmt func)
--        popSymTab

            -- check return type
--            base <- baseTypeOf (funcRetty func)
--            case base of
--                x | isSimple x -> return ()
--                TypeApply (Sym "Tuple") _ -> return ()
--                x -> error (show x)


pack :: [Node] -> [Node]
pack = Set.toList . Set.fromList


checkExpr :: Expr -> DoM CheckState Node
checkExpr (AExpr exprType expression) = withPos expression $ case expression of
    Int pos n -> return NodeNull
    AST.Float pos n -> return NodeNull
    AST.Bool pos b -> return NodeNull
    AST.String pos s -> return NodeNull
    AST.Char pos c -> return NodeNull

    AST.Array pos exprs -> do
        mapM_ checkExpr exprs
        return NodeNull

    -- returns the references of the expr
    Field pos expr ident -> do
        checkExpr expr

    -- returns one reference
    Ident pos symbol -> do
        look symbol

    -- return objects from mparam if returns record
    Call pos symbol exprs -> do
        return NodeNull
--
--        ast <- gets astResolved
--        unless (symbolIsResolved symbol) $ fail ("unresolved function call: " ++ prettySymbol symbol)
--
--        let params = funcArgs $ (getInstanceHeader symbol ast)
--        let retty  = funcRetty $ (getInstanceHeader symbol ast)
--
--        nodes <- mapM checkExpr exprs
--        unless (length nodes == length params) (fail "length error")
--
--        nodes' <- fmap catMaybes $ forM (zip nodes params) $ \(node, param) -> do
--            case param of
--                RefParam _ _ _ -> return (Just node)
--                Param _ _ _    -> return Nothing
--
--        checkNoSameBaseNodes nodes'
--
--        -- check explicit reference args
--        forM_ (zip params exprs) $ \(param, expr) -> case param of
--            Param _ _ _ -> return ()
--            RefParam _ _ _ -> case expr of
--                AExpr _ (Reference _ _) -> return ()
--                _                       -> fail "specify reference args with '&'"
--        
--        case retty of
--            RefRetty _ -> return (NodeUnion nodes')
--            Retty _    -> return NodeNull
        --return NodeNull -- TODO

    -- pat defines, redefines references of expr.
    Match pos expr pat -> do
        --liftIO $ putStrLn "match"
        checkExpr expr
        checkPattern pat
        return NodeNull

    AST.Reference pos expr -> checkExpr expr

    x -> fail ("unknown expression: " ++ show x)

checkExpr x = fail $ "unresolved type: " ++ show x
    


checkStmt :: Stmt -> DoM CheckState ()
checkStmt stmt = withPos stmt $ case stmt of
    Block stmts -> do
        pushSymTab
        mapM_ checkStmt stmts
        popSymTab

    If _ cnd blk mblk -> do
        --liftIO $ putStrLn $ "If"
        --pushSymTab
        checkExpr cnd
        checkStmt blk
        traverse checkStmt mblk
        return ()
        --popSymTab

    Return _ mexpr -> do
        void $ traverse checkExpr mexpr

    ExprStmt expr -> do
        checkExpr expr
        return ()

    EmbedC _ _ -> do
        return ()

    Data _ symbol typ Nothing -> do
        define symbol (NodeData symbol)
        return ()

    Let _ pat Nothing Nothing -> do
        checkPattern pat 
        return ()

    While _ cnd blk -> do
        checkExpr cnd
        checkStmt blk
        return ()

    Assign pos symbol expr -> do
        define symbol (NodeDefine symbol)
        checkExpr expr
        return ()


    x -> error "invalid statement"


checkPattern :: Pattern -> DoM CheckState Node
checkPattern (PatAnnotated pattern patType) = withPos pattern $ case pattern of
    PatIdent _ symbol -> do
        --liftIO $ putStrLn $ "defining: " ++ show symbol
        define symbol (NodeDefine symbol)
        return NodeNull

    x -> error (show x)
checkPattern pattern = withPos pattern $ fail $ "unresolved pattern: " ++ show pattern
