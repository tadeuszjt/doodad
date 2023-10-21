{-# LANGUAGE FlexibleContexts #-}
module TupleDeleter where

import qualified Data.Map as Map
import Control.Monad.State

import AST
import ASTMapper
import ASTResolved
import Monad
import Type
import Error


mapper :: BoM ASTResolved m => Elem -> m (Maybe Elem)
mapper elem = case elem of
    ElemType (Type.Tuple (TypeApply symbol ts)) -> do
        resm <- Map.lookup symbol <$> gets typeFuncs
        case resm of
            Nothing -> return (Just elem) -- symbol isn't defined, must be generic
            Just (ss, typ) -> do
                assert (length ts == length ss) "invalid number of type arguments"
                let applied = applyTypeFunction ss ts typ
                case applied of
                    Record _ -> return (Just elem)
                    _ -> error (show applied)

    ElemType (Type.Tuple (Record _))     -> return (Just elem)

    ElemType (Type.Tuple t) | isSimple t -> do
        liftIO $ putStrLn $ "deleted: " ++ show (Type.Tuple t)
        return $ Just $ ElemType t

    ElemType (Type.Tuple (Type.Tuple t)) -> do
        liftIO $ putStrLn $ "deleted: " ++ show (Type.Tuple (Type.Tuple t))
        return $ Just $ ElemType (Type.Tuple t)

    ElemType (Type.Tuple _) -> error (show elem)

    ElemType _ -> return (Just elem)
    ElemExpr _ -> return (Just elem)
    ElemPattern _ -> return (Just elem)
    ElemStmt _ -> return (Just elem)

    _ -> error (show elem)




deleteSingleTuples :: BoM ASTResolved m => m ()
deleteSingleTuples = do
    funcDefs'  <- mapM (mapFuncBody mapper) =<< gets funcDefs
    typeFuncs' <- mapM (\(ss, t) -> do { t' <- mapType mapper t; return (ss, t')}) =<< gets typeFuncs
    modify $ \s -> s { funcDefs = funcDefs', typeFuncs = typeFuncs' }
