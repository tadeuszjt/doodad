module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import AST
import Type
import Constraint
import Monad
import Error
import Control.Monad.State
import qualified SymTab
import Symbol
import ASTResolved
import ASTMapper
import FunctionFinder


type SymTab = SymTab.SymTab Symbol () Object
data Object
    = ObjVar Type
    | ObjConst Expr
    deriving (Show, Eq)

data CollectState
    = CollectState
        { symTab      :: SymTab
        , curRetty    :: Type
        , collected   :: Map.Map Constraint TextPos
        , defaults    :: Map.Map Constraint TextPos
        , curPos      :: TextPos
        , astResolved :: ASTResolved
        }

initCollectState astResolved = CollectState
    { symTab      = SymTab.initSymTab
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
    , astResolved = astResolved 
    }


collectPos :: (TextPosition t) => t -> DoM CollectState a -> DoM CollectState a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r


collect :: Constraint -> DoM CollectState ()
collect constraint =
    modify $ \s -> s { collected = Map.insert (constraint) (curPos s) (collected s) }

collectEq :: Type -> Type -> DoM CollectState ()
collectEq t1 t2 = collect (ConsEq t1 t2)

collectDefault :: Type -> Type -> DoM CollectState ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = Map.insert (ConsEq t1 t2) (curPos s) (defaults s) }


look :: Symbol -> DoM CollectState Object
look symbol = do
    rm <- SymTab.lookup symbol () <$> gets symTab
    unless (isJust rm) (error $ show symbol ++ " undefined")
    return (fromJust rm)


define :: Symbol -> Object -> DoM CollectState ()
define symbol obj = do
    resm <- SymTab.lookupHead symbol () <$> gets symTab
    unless (isNothing resm) (error $ show symbol ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert symbol () obj (symTab s) }


collectAST :: Prelude.Bool -> ASTResolved -> DoM CollectState ()
collectAST verbose ast = do
    --when verbose $ liftIO $ putStrLn "collecting..."
    forM (Map.toList $ constDefs ast) $ \(symbol, expr) -> do
        define symbol (ObjConst expr)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcTypeArgs body == []) $
            collectFuncDef symbol body


collectCall :: Type -> [Expr] -> Symbol -> [Expr] -> DoM CollectState ()
collectCall exprType params symbol args = do -- can be resolved or sym
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- gets astResolved
    mReceiverType <- case params of
        [] -> return Nothing
        [p] -> return (Just $ typeof p)
    candidates <- fmap fst $ runDoMExcept ast (findCandidates mReceiverType symbol (map typeof args) exprType)
    case candidates of
        [symbol] | isGenericFunction symbol ast -> return ()
        [symbol] | isNonGenericFunction symbol ast -> do
            let header = getFunctionHeader symbol ast
            collectEq exprType (returnType header)
            zipWithM_ collectEq (map typeof params) (paramTypes header)
            zipWithM_ collectEq (map typeof args) (argTypes header)

        [symbol] | isCtor symbol ast -> return ()

        _ -> return ()


collectFuncDef :: Symbol -> FuncBody -> DoM CollectState ()
collectFuncDef symbol body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = funcRetty body }
    forM (funcParams body) $ \(Param _ symbol t) -> define symbol (ObjVar t)
    forM_ (funcArgs body) $ \(Param _ symbol t) -> define symbol (ObjVar t)
    mapStmtM collectMapper (funcStmt body)
    modify $ \s -> s { curRetty = oldRetty }
    --collectDefault (funcRetty body) Void
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


collectMapper :: Elem -> DoM CollectState Elem
collectMapper element = (\_ -> return element) =<< case element of
    ElemStmt statement -> case statement of
        Increment _ _          -> return ()
        Block _                -> return ()
        EmbedC _ _             -> return ()
        ExprStmt expr          -> collectDefault (typeof expr) Void
        If _ expr blk melse    -> collect $ ConsBase Type.Bool (typeof expr)
        Let _ pattern mexpr _  -> when (isJust mexpr) $ collectEq (typeof pattern) (typeof $ fromJust mexpr)
        SetOp _ op expr1 expr2 -> collectEq (typeof expr1) (typeof expr2)
        While _ expr _         -> collect $ ConsBase Type.Bool (typeof expr)
        Return _ mexpr         -> collectEq (maybe Void typeof mexpr) =<< gets curRetty

        Switch p expr cases    -> forM_ cases $ \(pat, _) ->
            collectEq (typeof pat) (typeof expr)

        For p expr mpat blk -> when (isJust mpat) $ do
            collect $ ConsSubscript (typeof expr) (typeof $ fromJust mpat)

        Data p symbol typ mexpr -> do
            define symbol (ObjVar typ)
            void $ traverse (collectEq typ . typeof) mexpr

    ElemPattern (PatAnnotated pattern patType) -> case pattern of
        PatIgnore _           -> return ()
        PatNull _             -> return ()
        PatIdent _ symbol     -> define symbol (ObjVar patType)
        PatTypeField _ t _    -> collectEq patType t
        PatLiteral expr       -> collectEq patType (typeof expr)
        PatGuarded _ pat expr -> collect $ ConsBase Type.Bool (typeof expr)
        PatRecord _ pats      -> collect $ ConsRecord patType (map typeof pats)

        PatTuple _ pats -> do
            collectDefault patType (Type.Tuple $ Type.Record $ map typeof pats)
            collect $ ConsTuple patType (map typeof pats)

        PatAnnotated pat t -> do
            collectEq t patType
            collectEq t (typeof pat)

        PatField _ symbol pats -> do
            ast <- gets astResolved
            candidates <- fmap fst $ runDoMExcept ast (findCtorCandidates symbol)
            symbol' <- case candidates of
                [s] -> return s
                xs  -> error $ "PatField candidates for: " ++ show symbol ++ " " ++ show xs

            (s, i) <- mapGet symbol' . ctorDefs =<< gets astResolved
            collect $ ConsAdtField patType i (map typeof pats)

    ElemExpr (AExpr exprType expression) -> case expression of
        Call _ params symbol exprs -> collectCall exprType params symbol exprs
        Prefix _ op expr    -> collectEq exprType (typeof expr)
        Int _ _             -> collectDefault exprType I64
        Float _ _           -> collectDefault exprType F64
        Subscript _ e1 e2   -> collect $ ConsSubscript (typeof e1) exprType
        RecordAccess _ expr -> collect $ ConsRecordAccess exprType (typeof expr) 
        Null _              -> return ()
        Field _ e symbol    -> collect $ ConsField (typeof e) symbol exprType
        AST.Record _ exprs  -> do
            collect $ ConsBase exprType (Type.Record $ map typeof exprs)
            collectDefault exprType (Type.Record $ map typeof exprs)

        AST.Char _ _ -> do
            collect (ConsBase exprType Type.Char)
            collectDefault exprType Type.Char

        AST.Bool _ _ -> do
            collect (ConsBase exprType Type.Bool)
            collectDefault exprType Type.Bool

        AST.String _ _        -> do
            collect (ConsBase exprType Type.String)
            collectDefault exprType Type.String

        Construct _ symbol args -> do
            (typeSymbol, i)    <- mapGet symbol =<< gets (ctorDefs . astResolved)
            (generics, ADT ts) <- mapGet typeSymbol =<< gets (typeFuncs . astResolved)
            case exprType of
                Type _ -> return ()

                TypeApply s _ | s == typeSymbol -> do
                    case args of
                        []    -> unless ( (ts !! i) == Void ) (error "type wasn't void")
                        [arg] -> collect $ ConsAdtField exprType i [typeof arg]
                        args  -> collect $ ConsAdtField exprType i (map typeof args)

                _ -> return () -- TODO
                _ -> error (show exprType)

        Builtin _ sym args -> do 
            case sym of
                "conv"  -> return ()
                "assert" -> do
                    check (length args == 2) "invalid assert args"
                    collect $ ConsBase (typeof $ args !! 0) Type.Bool
                    collect $ ConsBase (typeof $ args !! 1) Type.String
                    collectEq exprType Void
                "len"   -> do
                    collect (ConsBase exprType I64)
                    collectDefault exprType I64
                "print" -> collectEq exprType Void

        Ident _ symbol -> do
            obj <- look symbol 
            case obj of
                ObjVar typ -> collectEq typ exprType
                ObjConst e -> do -- special!
                    return ()
    --                count <- gets typeSupply
    --                (e', count') <- runDoMExcept count (annotate e)
    --                modify $ \s -> s { typeSupply = count' }
    --                collectEq (typeof e') exprType
    --                collectExpr e'
                    
        Infix _ op e1 e2 -> do
            case op of
                _ | op `elem` [Plus, Minus, Times, Divide, Modulo] -> do
                    collectEq exprType (typeof e1)
                _ | op `elem` [AST.LT, AST.GT, AST.LTEq, AST.GTEq, AST.EqEq, AST.NotEq]  -> do
                    collect (ConsBase exprType Type.Bool)
                    collectDefault exprType Type.Bool
                _ | op `elem` [AndAnd, OrOr] -> do
                    collect (ConsBase exprType Type.Bool)
                    collectEq exprType (typeof e1)
                _ -> return ()
            collectEq (typeof e1) (typeof e2)

        AST.Tuple _ exprs -> do
            collect $ ConsTuple exprType (map typeof exprs)
            collectDefault exprType $ Type.Tuple $ Type.Record (map typeof exprs)

        Match _ e p -> do
            collectEq (typeof p) (typeof e)
            collectDefault exprType Type.Bool

        AST.Range _ me me1 me2 -> do
            when (isJust me1 && isJust me2) $ do
                collectEq (typeof $ fromJust me1) (typeof $ fromJust me2)
            when (isJust me1) $ do
                collect $ ConsBase (Type.Range $ typeof $ fromJust me1) exprType
                collectDefault (Type.Range $ typeof $ fromJust me1) exprType
            when (isJust me2) $ do
                collect $ ConsBase (Type.Range $ typeof $ fromJust me2) exprType
                collectDefault (Type.Range $ typeof $ fromJust me2) exprType
            when (isNothing me1 && isNothing me2) $ do
                collectDefault (Type.Range I64) exprType

    ElemExpr _ -> return ()
    ElemPattern _ -> return ()
    ElemStmt _ -> return ()
    ElemType _ -> return ()

