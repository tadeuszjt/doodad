{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IRGen where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Symbol
import Monad
import Error
import Type
import Interop
import States


prettyIrGenState :: IRGenState -> IO ()
prettyIrGenState irGenState = do
    putStrLn $ "module: " ++ irModuleName irGenState
    forM_ (Map.toList $ irTypeDefs irGenState) $ \(symbol, anno) -> do
        putStrLn $ "type: " ++ show symbol ++ "\t" ++ show anno

    forM_ (Map.toList $ irCtorDefs irGenState) $ \(symbol, (t, i)) -> do
        putStrLn $ "ctor: " ++ show symbol ++ "\t" ++ show t

    forM_ (Map.toList $ irExternDefs irGenState) $ \(name, (pts, sym, ats, rt)) -> do
        putStrLn $ "extern: " ++ AST.brcStrs (map show pts) ++ " " ++ sym ++ AST.tupStrs (map show ats) ++ " " ++ show rt ++ " " ++ show name

    when (isJust $ irMainDef irGenState) $ do
        putStrLn $ "main: " ++ (show $ length $ funcStmts $ fromJust $ irMainDef irGenState)

    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, body) -> do
        putStrLn $ "func: " ++ show symbol
        putStrLn $ "    params: " ++ AST.brcStrs (map show $ funcParams body)
        putStrLn $ "    args:   " ++ AST.tupStrs (map show $ funcArgs body)
        putStrLn ""


initIRGenState moduleName = IRGenState
    { irModuleName     = moduleName
    , irTypeDefs       = Map.empty
    , irExternDefs     = Map.empty
    , irFuncDefs       = Map.empty
    , irCtorDefs       = Map.empty
    , irFuncMap        = Map.empty
    , irTypeMap        = Map.empty
    , irCtorMap        = Map.empty
    , irMainDef        = Nothing
    }


compile :: BoM IRGenState m => ResolvedAst -> m ()
compile ast = do
    modify $ \s -> s { irTypeDefs = Map.union (typeDefs ast) (typeImports ast) }
    modify $ \s -> s { irExternDefs = funcImports ast }
    modify $ \s -> s { irCtorDefs = Map.union (ctorImports ast) (ctorDefs ast) }
    modify $ \s -> s { irTypeMap  = Map.mapKeys sym $ Map.mapWithKey (\k a -> k) (typeDefs ast) }
    modify $ \s -> s { irCtorMap  = Map.mapKeys sym $ Map.mapWithKey (\k a -> k) (ctorDefs ast) }

    initialiseTupleMembers =<< gets irCtorDefs
    initialiseTopFuncDefs (funcDefs ast)
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        compileFuncDef symbol body


initialiseTopFuncDefs :: BoM IRGenState m => Map.Map Symbol FuncBody -> m ()
initialiseTopFuncDefs funcDefs = do
    forM_ (Map.toList funcDefs) $ \(symbol, funcBody) ->
        case sym symbol of
            "main" -> do
                Nothing <- gets irMainDef
                modify $ \s -> s { irMainDef = Just (funcBody { funcStmts = [] }) }
            _ -> do
                False <- Map.member symbol <$> gets irFuncDefs
                let key = (map typeof (funcParams funcBody), sym symbol, map typeof (funcArgs funcBody), (funcRetty funcBody))
                modify $ \s -> s { irFuncMap = Map.insert  key symbol (irFuncMap s) }
                modify $ \s -> s { irFuncDefs = Map.insert symbol (funcBody { funcStmts = [] }) (irFuncDefs s) }


initialiseTupleMembers :: BoM IRGenState m => Map.Map Symbol (Type, Int) -> m ()
initialiseTupleMembers ctorDefs = do
    forM_ (Map.toList ctorDefs) $ \(symbol, (Type.Typedef symbolType, i)) -> do
        Just typ <- Map.lookup symbolType <$> gets irTypeDefs
        case typ of
            _-> return ()


compileFuncDef :: BoM IRGenState m => Symbol -> FuncBody -> m ()
compileFuncDef symbol body = do
    case sym symbol of 
        "main" -> modify $ \s -> s { irMainDef = Just body }
        _      -> modify $ \s -> s { irFuncDefs = Map.insert symbol body (irFuncDefs s) }
