{-# LANGUAGE FlexibleInstances #-}
module MakeFuncIR where

import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Data.Char

import Monad
import ASTResolved
import Symbol
import Type
import FindFunc
import Error
import IR
import AstBuilder
import InstBuilder hiding (ID, appendStmt, generateId)
import qualified AST as S


data FuncIRState = FuncIRState
    { symbolTable :: Map.Map Symbol ID
    , irFunc :: FuncIR
    , astResolved :: ASTResolved
    , rettyIr  :: RettyIR
    }


initFuncIRState ast = FuncIRState
    { symbolTable = Map.empty
    , irFunc = initFuncIr
    , astResolved = ast
    , rettyIr = RettyIR Value Tuple
    }


instance MonadFuncIR (DoM FuncIRState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets irFunc
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { irFunc = irFunc' }
        return a


define :: Symbol -> ID -> DoM FuncIRState ()
define symbol id = do
    resm <- gets (Map.lookup symbol . symbolTable)
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symbolTable = Map.insert symbol id (symbolTable s) }


look :: Symbol -> DoM FuncIRState ID
look symbol = do
    resm <- gets (Map.lookup symbol . symbolTable)
    unless (isJust resm) (fail $ "symbol isn't defined: " ++ prettySymbol symbol)
    return (fromJust resm)


withCurrentID :: ID -> (DoM FuncIRState a) -> DoM FuncIRState a
withCurrentID id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }
    return a


makeFuncIR :: TopStmt -> DoM FuncIRState (FuncIrHeader, FuncIR)
makeFuncIR (TopInst _ [] funcType args retty inst) = do
    let (TypeDef funcSymbol, _) = unfoldType funcType

    irParams <- forM args $ \param -> do
        (refType, typ, symbol) <- case param of
            S.Param _ symbol (Apply Type.Slice typ) -> return (IR.Slice, typ, symbol)
            S.Param _ symbol typ    -> return (Value, typ, symbol)
            S.RefParam _ symbol typ -> return (IR.Ref, typ, symbol)

        id <- generateId
        define symbol id
        addType id typ refType
        return (ParamIR refType typ)

    rettyIr <- case retty of
        S.Retty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty t -> return (RettyIR IR.Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { rettyIr = rettyIr }

    let Just (S.Block stmts) = Map.lookup 0 (statements inst)
    mapM_ (makeStmt inst) stmts

    state <- get
    let funcIrHeader = FuncIrHeader
            { irRetty     = rettyIr
            , irArgs      = irParams
            , irFuncSymbol = funcSymbol
            }
    return (funcIrHeader, irFunc state)



makeStmt :: InstBuilderState -> S.Stmt -> DoM FuncIRState ()
makeStmt inst (S.Stmt stmtId) = do
    let Just statement = Map.lookup stmtId (statements inst)
    case statement of
        S.Block stmts -> do
            id <- appendStmt (Block [])
            withCurrentID id (mapM_ (makeStmt inst) stmts)

        S.EmbedC _ strMap str -> do
            strMap' <- forM strMap $ \(s, symbol) -> do
                id' <- look symbol
                return (s, id')
            uses <- mapM look (map snd strMap)
            id <- appendStmt (EmbedC strMap' str)
            addTextPos id (textPos statement)

        S.Let _ pattern (Just expr) Nothing -> do
            curId <- getCurrentId
            b <- makePattern curId inst pattern =<< makeVal inst expr
            void $ call (Sym ["assert", "assert"]) [] [b]

        S.Let pos (S.Pattern patId) Nothing Nothing -> do
            let Just (S.PatIdent _ symbol) = Map.lookup patId (patterns inst)
            let Just typ                   = Map.lookup patId (types inst)
            id <- appendSSA typ Value (InitVar Nothing)
            addTextPos id pos
            define symbol id

        S.Assign _ symbol expr@(S.Expr ei) -> do
            case typeOfExpr inst expr of
                Apply Type.Slice t -> do
                    arg@(ArgID id) <- makeSlice inst expr
                    define symbol id
                _ -> do
                    arg <- (makeVal inst) expr
                    case arg of
                        ArgConst typ const -> define symbol =<< appendSSA typ Value (InitVar $ Just arg)
                        ArgID id -> define symbol id
                        x -> error (show x)

        S.Return _ (Just expr) -> do
            retty <- gets rettyIr
            case retty of
                RettyIR Value (Apply Type.Slice _) -> fail "val to slice"
                RettyIR IR.Ref   (Apply Type.Slice _) -> fail "ref to slice"
                RettyIR IR.Slice typ -> void $ appendStmt . Return =<< makeSlice inst expr
                RettyIR IR.Ref   typ -> void $ appendStmt . Return . ArgID =<< (makeRef inst) expr
                RettyIR Value typ -> void $ appendStmt . Return =<< (makeVal inst) expr
                x -> error (show x)

        S.Return _ Nothing     -> void $ appendStmt $ Return (ArgConst Tuple (ConstTuple []))-- void $ appendStmt ReturnVoid
        S.ExprStmt expr        -> void $ makeVal inst expr

        S.While pos expr (S.Stmt stmtId) -> do
            id <- appendStmt $ Loop []
            addTextPos id pos
            withCurrentID id $ do
                cnd <- (makeVal inst) expr

                not <- call (Sym ["boolean", "not"]) [Type.Bool] [cnd]

                ifId <- appendStmt (If not [])
                withCurrentID ifId (appendStmt Break)

                let Just (S.Block stmts) = Map.lookup stmtId (statements inst)
                mapM_ (makeStmt inst) stmts


        S.If pos expr (S.Stmt trueId) mfalse -> do
            curId <- getCurrentId
            arg <- makeCondition curId inst expr

            loopId <- appendStmt (Loop [])
            void $ withCurrentID loopId $ do
                -- make if
                ifId <- appendStmt (If arg [])
                let Just (S.Block trueStmts) = Map.lookup trueId (statements inst)
                withCurrentID ifId $ do
                    mapM_ (makeStmt inst) trueStmts
                    appendStmt Break

                case mfalse of
                    Nothing -> return ()
                    Just (S.Stmt falseId) -> do
                        let Just (S.Block falseStmts) = Map.lookup falseId (statements inst)
                        mapM_ (makeStmt inst) falseStmts

                appendStmt Break

        S.Switch pos expr cases -> do
            arg <- makeVal inst expr

            loopId <- appendStmt (Loop [])
            void $ withCurrentID loopId $ do
                forM_ cases $ \(pat, stmt) -> do
                    b <- makePattern loopId inst pat arg

                    ifId <- appendStmt (If b [])
                    withCurrentID ifId $ do
                        makeStmt inst stmt
                        appendStmt Break

                call (Sym ["assert", "assert"]) [] [ArgConst Type.Bool (ConstBool False)]
                appendStmt Break


        x -> error (show x)



makeCondition :: ID -> InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeCondition defBlkId inst (S.Expr exprId) = case expressions inst Map.! exprId of
    S.Match _ (S.Expr id) pat -> do
        makePattern defBlkId inst pat =<< case expressions inst Map.! id of
            S.Match _ _ _ -> makeCondition defBlkId inst (S.Expr id)
            _             -> makeVal inst (S.Expr id)

    expr -> makeVal inst (S.Expr exprId)
    


makePattern :: ID -> InstBuilderState -> S.Pattern -> Arg -> DoM FuncIRState Arg
makePattern defBlkId inst (S.Pattern patId) arg = case patterns inst Map.! patId of
    S.PatIgnore _ -> return $ ArgConst Type.Bool (ConstBool True)

    S.PatLiteral expr@(S.Expr exprId) -> do
        expr' <- makeVal inst expr
        let Just exprType = Map.lookup exprId (types inst)
        call (Sym ["builtin", "builtinEqual"]) [exprType] [arg, expr']

    S.PatField _ str pats -> do
        let patType = typeOfPat inst (S.Pattern patId)
        ref <- appendSSA patType IR.Ref (MakeReferenceFromValue arg)
        isCase <- call (Sym ["is" ++ toUpper (head str) : tail str]) [patType] [ArgID ref]

        case pats of
            [] -> return isCase
            [pat] -> do
                match <- appendSSA Type.Bool Value $ InitVar $ Just $ ArgConst Type.Bool (ConstBool False)
                ifId <- appendStmt (If isCase [])
                withCurrentID ifId $ do
                    let fieldType = typeOfPat inst pat

                    let (TypeDef _, generics) = unfoldType patType
                    fieldRef <- call (Sym ["from" ++ toUpper (head str) : tail str]) generics [ArgID ref]
                    b <- makePattern defBlkId inst pat =<<
                        copy . ArgID =<<
                            appendSSA fieldType Value (MakeValueFromReference fieldRef)
                    matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue $ ArgID match)
                    call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, b]

                return (ArgID match)

            _ -> fail "todo"

    
    S.PatIdent pos symbol -> do
        let patType = typeOfPat inst (S.Pattern patId)
        var <- withCurrentID defBlkId $
            prependSSA patType Value (InitVar Nothing)
        define symbol var

        ref <- appendSSA patType IR.Ref (MakeReferenceFromValue $ ArgID var)
        call (Sym ["builtin", "store"]) [patType] [ArgID ref, arg]
        return $ ArgConst Bool (ConstBool True)

    S.PatGuarded pos pat expr -> do
        patMatch <- makePattern defBlkId inst pat arg
        match <- appendSSA Type.Bool Value $ InitVar $ Just $ ArgConst Type.Bool (ConstBool False)

        ifId <- appendStmt (If patMatch [])
        withCurrentID ifId $ do
            matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue $ ArgID match)
            b <- makeCondition defBlkId inst expr
            call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, b]

        return (ArgID match)


    S.PatTuple pos pats -> do
        match <- appendSSA Type.Bool Value $ InitVar $ Just $ ArgConst Type.Bool (ConstBool True)
        matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue $ ArgID match)
        loopId <- appendStmt (Loop [])

        withCurrentID loopId $ do
            forM_ (zip pats [0..]) $ \(pat@(S.Pattern fieldId), i) -> do

                let Just patType   = Map.lookup patId (types inst)
                let Just fieldType = Map.lookup fieldId (types inst)

                ref <- appendSSA patType IR.Ref (MakeReferenceFromValue arg)
                field <- call
                    (Sym ["tuple", "tuplePattern"])
                    [fieldType, Size (length pats), Size i, patType]
                    [ArgID ref]

                fieldVal <- appendSSA fieldType Value (MakeValueFromReference field)

                b <- makePattern defBlkId inst pat =<< copy (ArgID fieldVal)
                not <- call (Sym ["boolean", "not"]) [Type.Bool] [b]

                ifId <- appendStmt (If not [])
                withCurrentID ifId $ do
                    call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, ArgConst Type.Bool (ConstBool False)]
                    appendStmt Break

            appendStmt Break

        return (ArgID match)

    x -> error (show x)


makeSlice :: InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeSlice inst (S.Expr exprId) = do
    let Just expression = Map.lookup exprId (expressions inst)
    let Just exprType   = Map.lookup exprId (types inst)

    withPos expression $ case expression of
        S.String _ str -> do
            let Apply Type.Slice typ = exprType
            fmap ArgID $ appendSSA typ IR.Slice (MakeString str)

        S.Ident _ symbol -> do
            id <- look symbol
            (typ, IR.Slice) <- getType (ArgID id)
            return (ArgID id)

        S.Reference _ expr -> makeSlice inst expr

        S.Array _ exprs -> do
            let Apply Type.Slice t = exprType
            fmap ArgID $ appendSSA t IR.Slice . MakeSlice =<< mapM copy =<< mapM (makeVal inst) exprs

        S.Call pos (Type funcTypeId) exprs -> do
            arg <- makeCall inst (S.Call pos (Type funcTypeId) exprs)
            (t, IR.Slice) <- getType arg
            return arg

        x -> error (show x)


makeCall :: InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeCall inst (S.Call pos (Type funcTypeId) exprs) = do
    let Just funcType           = Map.lookup funcTypeId (types inst)
    let (TypeDef funcSymbol, _) = unfoldType funcType

    ast <- gets astResolved
    resm <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
    irHeader <- case resm of
        Just x -> return x
        Nothing -> fail ("no instance for: " ++ show funcType)
    unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

    args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
        case arg of
            ParamIR _ (Apply Type.Slice _) -> error "there"

            ParamIR IR.Slice _ -> makeSlice inst expr
            ParamIR IR.Ref _ -> ArgID <$> (makeRef inst) expr
            ParamIR Value argType -> do
                if symbolsCouldMatch funcSymbol (Sym ["builtin", "copy"]) then (makeVal inst) expr
                else copy =<< (makeVal inst) expr

    case irRetty irHeader of
        RettyIR Value typ -> fmap ArgID $ appendSSA typ Value (Call funcType args)
        RettyIR IR.Ref typ -> fmap ArgID $ appendSSA typ IR.Ref (Call funcType args)
        RettyIR IR.Slice typ -> fmap ArgID $ appendSSA typ IR.Slice (Call funcType args)
        x -> error (show x)


-- returns either a Value or a Const ID
makeVal :: InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeVal inst (S.Expr exprId) = do
    let Just expression = Map.lookup exprId (expressions inst)
    let Just exprType   = Map.lookup exprId (types inst)

    withPos expression $ case expression of
        S.Bool _ b -> return (ArgConst exprType $ ConstBool b)
        S.Char _ c -> return (ArgConst exprType $ ConstChar c)
        S.Int _  n -> return (ArgConst exprType $ ConstInt n)
        S.Float _ f -> return $ ArgConst exprType (ConstFloat f)
        S.Reference _ expr -> (makeVal inst) expr

        S.Ident _ symbol -> do
            id <- look symbol
            (typ, refType) <- getType (ArgID id)
            case refType of
                Value -> return (ArgID id)
                IR.Ref   -> fmap ArgID $ appendSSA typ Value (MakeValueFromReference (ArgID id))
                x -> fail "here"

        S.Call pos (Type funcTypeId) exprs -> do
            arg <- makeCall inst (S.Call pos (Type funcTypeId) exprs)
            (t, refType) <- getType arg
            case refType of
                Value -> return arg
                IR.Ref -> fmap ArgID (appendSSA t Value $ MakeValueFromReference arg)

        x -> error (show x)


-- returns a Ref ID
makeRef :: InstBuilderState -> S.Expr -> DoM FuncIRState ID
makeRef inst (S.Expr exprId) = do
    let Just expression = Map.lookup exprId (expressions inst)
    let Just exprType   = Map.lookup exprId (types inst)

    withPos expression $ case expression of
        S.Reference _ expr -> makeRef inst expr
        S.Ident _ symbol -> do
            id <- look symbol
            (typ, refType) <- getType (ArgID id)
            --TODO these may be different because of lower functions
            --unless (typ == exprType) (fail $ "type mismatch: " ++ show typ ++ ", " ++ show exprType)

            case refType of
                Value -> appendSSA typ IR.Ref (MakeReferenceFromValue $ ArgID id)
                IR.Ref -> return id
                x -> error (show x)

        S.Call pos (Type funcTypeId) exprs -> do
            arg@(ArgID argId) <- makeCall inst (S.Call pos (Type funcTypeId) exprs)
            (t, refType) <- getType arg
            case refType of
                IR.Ref -> return argId
                Value  -> appendSSA t Value (MakeReferenceFromValue arg)

        x -> error (show x)


copy :: Arg -> DoM FuncIRState Arg
copy arg = do
    (t, _) <- getType arg
    call (Sym ["builtin", "copy"]) [t] [arg]


call :: Symbol -> [Type] -> [Arg] -> DoM FuncIRState Arg
call symbol@(Sym _) funcTypeArgs args = do
    ast <- gets astResolved

    symbol' <- case filter (symbolsCouldMatch symbol) (Map.keys (typeDefsAll ast)) of
        [] -> error "here"
        [x] -> return x
    let funcType = foldType (TypeDef symbol' : funcTypeArgs)

    resm <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
    irHeader <- case resm of
        Nothing -> fail $ "no instance: " ++ show funcType
        Just irHeader -> return irHeader

    unless (length (irArgs irHeader) == length args) (error "arg length mismatch")
    forM_ (zip args $ irArgs irHeader) $ \(arg, ParamIR paramType _) -> do
        (t, refType) <- getType arg
        case (refType, paramType) of
            (Value, Value) -> return ()
            (IR.Ref, IR.Ref) -> return ()
            (Const, Value) -> return ()
            (IR.Ref, Value) -> error (prettySymbol symbol')
                

            x -> fail (show x)

    case irRetty irHeader of
        RettyIR Value  t -> fmap ArgID $ appendSSA t Value (Call funcType args)
        RettyIR IR.Ref t -> fmap ArgID $ appendSSA t IR.Ref (Call funcType args)
        x -> error (show x)


