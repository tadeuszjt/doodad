{-# LANGUAGE FlexibleInstances #-}
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
import Symbol
import ASTResolved
import FindFunc


data CollectState
    = CollectState
        { symTab      :: Map.Map Symbol Type
        , curRetty    :: Type
        , collected   :: Map.Map Constraint ConstraintInfo
        , defaults    :: Map.Map Constraint ConstraintInfo
        , curPos      :: TextPos
        , astResolved :: ASTResolved
        }

initCollectState ast = CollectState
    { symTab      = Map.empty
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
    , astResolved = ast
    }


instance TypeDefs (DoM CollectState) where
    getTypeDefs = gets (typeDefsAll . astResolved)


collectPos :: (TextPosition t) => t -> DoM CollectState a -> DoM CollectState a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r


collect :: String -> Constraint -> DoM CollectState ()
collect msg constraint = do
    curPos <- gets curPos
    let info = ConstraintInfo { infoTextPos = curPos, infoMsg = msg }
    modify $ \s -> s { collected = Map.insert (constraint) info (collected s) }


collectDefault :: Type -> Type -> DoM CollectState ()
collectDefault t1 t2 = do
    curPos <- gets curPos
    let info = ConstraintInfo { infoTextPos = curPos, infoMsg = "default" }
    modify $ \s -> s { defaults = Map.insert (ConsDefault t1 t2) info (defaults s) }


look :: Symbol -> DoM CollectState Type
look symbol = do
    --liftIO $ putStrLn $ "looking: " ++ prettySymbol symbol
    rm <- Map.lookup symbol <$> gets symTab
    unless (isJust rm) (fail $ prettySymbol symbol ++ " undefined")
    return (fromJust rm)


define :: Symbol -> Type -> DoM CollectState ()
define symbol obj = do
    --liftIO $ putStrLn $ "defining: " ++ prettySymbol symbol
    resm <- Map.lookup symbol <$> gets symTab
    unless (isNothing resm) (error $ prettySymbol symbol ++ " already defined")
    modify $ \s -> s { symTab = Map.insert symbol obj (symTab s) }


collectStmt :: Stmt -> DoM CollectState ()
collectStmt statement = collectPos statement $ case statement of
    Acquires pos generics typ args isRef stmt -> do
        (Type.Func, retty : argTypes) <- unfoldType <$> baseTypeOf typ
        unless (length argTypes == length args) (fail "arg length mismatch")

        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }

        forM_ (zip args argTypes) $ \(arg, t) -> define (paramSymbol arg) t
        collectStmt stmt

        modify $ \s -> s { curRetty = oldRetty }

    Derives _ _ _ _ -> return ()
    EmbedC _ _ _ -> return ()

    Block stmts -> mapM_ collectStmt stmts

    Return _ mexpr -> do
        curRetty <- gets curRetty
        when (isJust mexpr && curRetty == Void) (fail "cannot return in void function")
        collect "return type must match function return type" $
            ConsEq (maybe Void typeof mexpr) curRetty
        void $ traverse collectExpr mexpr

    ExprStmt expr -> collectExpr expr

    Let _ pattern mexpr Nothing  -> do
        when (isJust mexpr) $ do
            collect "let type must match expression type" $
                ConsEq (typeof pattern) (typeof $ fromJust mexpr)
            collectExpr (fromJust mexpr)
        collectPattern pattern
        
    If _ expr blk melse -> do
        collect "if condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        void $ traverse collectStmt melse

    While _ expr blk -> do
        collect "while condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk

    Data _ symbol typ mexpr -> do
        define symbol typ
        void $ traverse (collect "data type must match expression type" . ConsEq typ . typeof) mexpr
        void $ traverse collectExpr mexpr

    Assign pos symbol expr -> do
        define symbol (typeof expr)
        collectExpr expr

    x -> do
        liftIO $ prettyStmt "" x
        error "invalid statement"


collectPattern :: Pattern -> DoM CollectState ()
collectPattern (PatAnnotated pattern patType) = collectPos pattern $ case pattern of
    PatIdent _ symbol     -> do
        define symbol patType

    x -> error (show x)


collectExpr :: Expr -> DoM CollectState ()
collectExpr (AExpr exprType expression) = collectPos expression $ case expression of
    Float _ _    -> collect "float is F64" (ConsEq exprType F64)
    AST.Bool _ _ -> collect "bool literal must have Bool type" (ConsEq exprType Type.Bool)
    Int _ _      -> collect "integer is type I64" (ConsEq exprType I64)
    AST.Char _ _ -> collect "char literal must have Char type" (ConsEq exprType Type.Char)

    Call _ callType exprs -> do
        -- TODO broken, generics are carrying thorugh definitions!
        let (TypeDef funcSymbol, callTypeArgs) = unfoldType callType
        (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
        unless (length exprs == length argTypes) (fail $ "invalid function type arguments: " ++ show callType)

        --liftIO $ putStrLn $ ("callType: " ++ show callType ++ ", retType: " ++ show retType)

        ast <- gets astResolved


        -- These indices describe the type variables which are independent according to the 
        -- functional dependencies. If the independent variables fully describe the call type,
        -- it can be said that no other overlapping definition could conflict with it so we can
        -- type check.
        let Feature _ featureGenerics funDeps _ _ _ = (featuresAll ast) Map.! funcSymbol
        unless (length callTypeArgs == length featureGenerics) (error "xs needs to be > 0")
        indices <- fmap catMaybes $ forM (zip featureGenerics [0..]) $ \(g, i) -> do
            case findIndex (g ==) (map snd funDeps) of
                Just _  -> return Nothing
                Nothing -> return (Just i) 

        fullAcqs <- fmap catMaybes $ forM (Map.elems $ acquiresAll ast) $ \stmt -> do
            appliedAcqType <- case stmt of
                Acquires _ generics acqType _ _ _ -> do
                    let genericsToVars = zip (map TypeDef generics) (map Type [-1, -2..])
                    return (applyType genericsToVars acqType)

                Derives pos generics argType [featureType] -> do
                    let acqType = Apply featureType argType
                    let genericsToVars = zip (map TypeDef generics) (map Type [-1, -2..])
                    return (applyType genericsToVars acqType)

            case typesCouldMatch appliedAcqType callType of
                False -> return Nothing
                True -> do
                    let (TypeDef _, acqTypeArgs) = unfoldType appliedAcqType
                    let b = all id $ map (\i -> typeFullyDescribes (acqTypeArgs !! i) (callTypeArgs !! i)) indices
                    case b of
                        True -> return (Just appliedAcqType) 
                        False -> return Nothing

        case fullAcqs of
            [] -> return ()
            [acq] -> do
                subs <- unify =<< getConstraintsFromTypes acq callType
                (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf (applyType subs acq)
                collect "call type" $ ConsEq callType (applyType subs acq)
                collect ("call return: " ++ show callType) (ConsEq exprType retType)
                zipWithM (\x y -> collect "call argument" (ConsEq x y)) argTypes (map typeof exprs)
                return ()
            x -> error (show x)
                
        when (Symbol.sym funcSymbol == "convert" && symbolModule funcSymbol == "convert") $ do
            unless (length argTypes == 1) (error "invalid")
            collectDefault exprType (argTypes !! 0)

        when (Symbol.sym funcSymbol == "make2" && symbolModule funcSymbol == "tuple") $ do
            unless (length argTypes == 2) (error "invalid")
            collectDefault exprType $ foldType [Tuple, argTypes !! 0, argTypes !! 1]
            
        when (Symbol.sym funcSymbol == "make3" && symbolModule funcSymbol == "tuple") $ do
            unless (length argTypes == 3) (error "invalid")
            collectDefault exprType $ foldType [Tuple, argTypes !! 0, argTypes !! 1, argTypes !! 2]

        when (Symbol.sym funcSymbol == "make4" && symbolModule funcSymbol == "tuple") $ do
            unless (length argTypes == 4) (error "invalid")
            collectDefault exprType $ foldType [Tuple, argTypes !! 0, argTypes !! 1, argTypes !! 2, argTypes !! 3]

        collect ("call return for: " ++ show callType) (ConsEq exprType retType)
        zipWithM (\x y -> collect "call argument" (ConsEq x y)) argTypes (map typeof exprs)
        mapM_ collectExpr exprs


    Match _ expr pat -> do
        collect "match must have same type for pattern and expression" $
            ConsEq (typeof pat) (typeof expr)
        collect "match type is Bool" $ ConsEq exprType Type.Bool
        collectExpr expr
        collectPattern pat

    AST.Reference _ expr -> do
        collect "reference type must match expression type" $ ConsEq exprType (typeof expr)
        collectExpr expr

    Ident _ symbol -> do
        typ <- look symbol 
        collect ("identifier type for " ++ prettySymbol symbol ++ " must match expression type") $
            ConsEq typ exprType

    AST.String _ s -> do
        collect "string literal must have Char.Slice type" $ ConsEq exprType (Apply Type.Slice Type.Char)

    AST.Array _ exprs -> do
        when (length exprs > 0) $ do
            forM_ (zip exprs [0..]) $ \(expr, i) ->
                collect "elements in array must have same type" $
                    ConsEq (typeof $ exprs !! i) (typeof $ head exprs)
            collect "array expression must have slice type" $
                ConsEq exprType $ Apply Type.Slice (typeof $ head exprs)
        mapM_ collectExpr exprs

    x -> error (show x)
