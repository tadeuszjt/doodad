module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
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
    | ObjConst S.Expr
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
collectEq t1 t2 = collect $ ConsEq t1 t2

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


collectAST :: Bool -> ASTResolved -> DoM CollectState ()
collectAST verbose ast = do
    --when verbose $ liftIO $ putStrLn "collecting..."
    forM (Map.toList $ constDefs ast) $ \(symbol, expr) -> do
        define symbol (ObjConst expr)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcTypeArgs body == []) $
            collectFuncDef symbol body


collectCall :: Type -> [S.Expr] -> Symbol -> [S.Expr] -> DoM CollectState ()
collectCall exprType params symbol args = do -- can be resolved or sym
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- gets astResolved
    candidates <- fmap fst $ runDoMExcept ast (findCandidates callHeader)
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
    forM (funcParams body) $ \(S.Param _ symbol t) -> define symbol (ObjVar t)
    forM_ (funcArgs body) $ \(S.Param _ symbol t) -> define symbol (ObjVar t)
    mapStmtM collectMapper (funcStmt body)
    modify $ \s -> s { curRetty = oldRetty }
    --collectDefault (funcRetty body) Void
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


collectMapper :: Elem -> DoM CollectState Elem
collectMapper element = (\_ -> return element) =<< case element of
    ElemType _ -> return ()

    ElemStmt stmt -> case stmt of
        S.Increment _ _          -> return ()
        S.Block _                -> return ()
        S.EmbedC _ _             -> return ()
        S.ExprStmt expr          -> collectDefault (typeof expr) Void
        S.If _ expr blk melse    -> collect $ ConsBase Bool (typeof expr)
        S.Let _ pattern expr _   -> collectEq (typeof pattern) (typeof expr)
        S.SetOp _ op expr1 expr2 -> collectEq (typeof expr1) (typeof expr2)
        S.While _ expr _         -> collect $ ConsBase Bool (typeof expr)
        S.Return _ mexpr         -> collectEq (maybe Void typeof mexpr) =<< gets curRetty

        S.Switch p expr cases    -> forM_ cases $ \(pat, _) ->
            collectEq (typeof pat) (typeof expr)

        S.For p expr mpat blk -> when (isJust mpat) $ do
            collect $ ConsSubscript (typeof expr) (typeof $ fromJust mpat)

        S.Data p symbol typ mexpr -> do
            define symbol (ObjVar typ)
            maybe (return ()) (collectEq typ . typeof) mexpr

    ElemPattern (S.PatAnnotated pattern patType) -> case pattern of
        S.PatIgnore _           -> return ()
        S.PatNull _             -> return ()
        S.PatIdent _ symbol     -> define symbol (ObjVar patType)
        S.PatTypeField _ t _    -> collectEq patType t
        S.PatLiteral expr       -> collectEq patType (typeof expr)
        S.PatGuarded _ pat expr -> collect $ ConsBase Bool (typeof expr)
        S.PatRecord _ pats      -> collect $ ConsRecord patType (map typeof pats)

        S.PatTuple _ pats -> do
            collectDefault patType (Tuple $ Record $ map typeof pats)
            collect $ ConsTuple patType (map typeof pats)

        S.PatAnnotated pat t -> do
            collectEq t patType
            collectEq t (typeof pat)

        S.PatField _ symbol pats -> do
            ast <- gets astResolved
            candidates <- fmap fst $ runDoMExcept ast (findCtorCandidates symbol)
            symbol' <- case candidates of
                [s] -> return s
                xs  -> error $ "PatField candidates for: " ++ show symbol ++ " " ++ show xs

            (s, i) <- mapGet symbol' . ctorDefs =<< gets astResolved
            collect $ ConsAdtField patType i (map typeof pats)
    ElemPattern _ -> return ()

    ElemExpr (S.AExpr exprType expression) -> case expression of
        S.Call _ params symbol exprs -> collectCall exprType params symbol exprs
        S.Prefix _ op expr    -> collectEq exprType (typeof expr)
        S.Int _ _             -> collectDefault exprType I64
        S.Float _ _           -> collectDefault exprType F64
        S.Subscript _ e1 e2   -> collect $ ConsSubscript (typeof e1) exprType
        S.RecordAccess _ expr -> collect $ ConsRecordAccess exprType (typeof expr) 
        S.Null _              -> return ()
        S.Field _ e symbol    -> collect $ ConsField (typeof e) symbol exprType

        S.Char _ _ -> do
            collect (ConsBase exprType Char)
            collectDefault exprType Char

        S.Bool _ _ -> do
            collect (ConsBase exprType Bool)
            collectDefault exprType Bool

        S.String _ _        -> do
            collect (ConsBase exprType String)
            collectDefault exprType String

        S.Construct _ symbol args -> do
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

        S.Builtin _ sym args -> do 
            case sym of
                "conv"  -> return ()
                "len"   -> do
                    collect (ConsBase exprType I64)
                    collectDefault exprType I64
                "print" -> collectEq exprType Void

        S.Ident _ symbol -> do
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
                    
        S.Infix _ op e1 e2 -> do
            case op of
                _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
                    collectEq exprType (typeof e1)
                _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> do
                    collect (ConsBase exprType Bool)
                    collectDefault exprType Bool
                _ | op `elem` [S.AndAnd, S.OrOr] -> do
                    collect (ConsBase exprType Bool)
                    collectEq exprType (typeof e1)
                _ -> return ()
            collectEq (typeof e1) (typeof e2)

        S.Tuple _ exprs -> do
            collect $ ConsTuple exprType (map typeof exprs)
            collectDefault exprType $ Tuple $ Record (map typeof exprs)

        S.Match _ e p -> do
            collectEq (typeof p) (typeof e)
            collectDefault exprType Bool

        S.Range _ me me1 me2 -> do
            when (isJust me1 && isJust me2) $ do
                collectEq (typeof $ fromJust me1) (typeof $ fromJust me2)
            when (isJust me1) $ do
                collect $ ConsBase (Range $ typeof $ fromJust me1) exprType
                collectDefault (Range $ typeof $ fromJust me1) exprType
            when (isJust me2) $ do
                collect $ ConsBase (Range $ typeof $ fromJust me2) exprType
                collectDefault (Range $ typeof $ fromJust me2) exprType
            when (isNothing me1 && isNothing me2) $ do
                collectDefault (Range I64) exprType
    ElemExpr _ -> return ()

    x -> error (show x)

