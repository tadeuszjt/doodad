{-# LANGUAGE FlexibleContexts #-}
module CPretty where

import Prelude hiding (print)
import Data.List
import qualified Data.Map as Map 
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import System.IO hiding (print)

import Monad
import CBuilder


data CPrettyState
    = CPrettyState
    { indent :: Int
    , fileHandle :: Handle
    , builder :: BuilderState
    }

initCPrettyState handle builder
    = CPrettyState
        { indent = 0
        , fileHandle = handle
        , builder = builder
        }



print :: BoM CPrettyState m => String -> m ()
print s = do
    printIndent
    handle <- gets fileHandle
    liftIO $ hPutStr handle s

printLn :: BoM CPrettyState m => String -> m ()
printLn s = do
    printIndent
    handle <- gets fileHandle
    liftIO $ hPutStrLn handle s


printIndent :: BoM CPrettyState m => m ()
printIndent = do
    level <- gets indent
    handle <- gets fileHandle
    void $ replicateM level $ liftIO $ hPutStr handle "    "

pushIndent :: BoM CPrettyState m => m ()
pushIndent = modify $ \s -> s { indent = indent s + 1 }

popIndent :: BoM CPrettyState m => m ()
popIndent = modify $ \s -> s { indent = (indent s - 1) }


cPretty :: BoM CPrettyState m => m ()
cPretty = do
    modName <- moduleName <$> gets builder
    printLn $ "/* Doodad Module: " ++ modName ++ " */"
    printLn "#include <stdio.h>"
    printLn "#include <assert.h>"
    printLn "#include <stdint.h>"
    printLn "#include <stdbool.h>"
    printLn "#include <gc.h>"

    elems <- elements <$> gets builder
    let global = elems Map.! (ID 0)
    forM_ (globalBody global) $ \id -> do
        cPrettyElem (elems Map.! id)

    printLn ""


cPrettyElem :: BoM CPrettyState m => Element -> m ()
cPrettyElem elem = case elem of
    func@(Func _ _ _ _) -> do
        printLn ""
        printLn $ show (funcRetty func) ++ " " ++ funcName func ++ "(" ++ intercalate ", " (map show $ funcArgs func) ++ ") {"
        pushIndent
        forM_ (funcBody func) $ \id -> do
            elem <- (Map.! id) . elements <$> gets builder
            cPrettyElem elem
        popIndent
        printLn $ "}"

    extern@(Extern _ _ _) -> do
        print $ "extern " ++ show (extRetty extern) ++ " " ++ extName extern ++ "("
        forM_ (zip (extArgs extern) [0..]) $ \(argType, i) -> do
            print (show argType)
            if i < length (extArgs extern) - 1 then print ", "
            else print ""
        printLn ");"

    typedef@(Typedef _ _) -> do
        printLn ""
        printLn $ "typedef " ++ show (typedefType typedef) ++ " " ++ typedefName typedef ++ ";"

    return@(Return expr) -> do
        printLn $ "return " ++ show expr ++ ";"

    assign@(Assign typ str expr) -> do
        printLn $ show typ ++ " " ++ str ++ " = " ++ show expr ++ ";"

    if_@(If _ _) -> do
        printLn $ "if (" ++ show (ifExpr if_) ++ ") {"
        pushIndent
        forM_ (ifStmts if_) $ \id -> do
            elem <- (Map.! id) . elements <$> gets builder
            cPrettyElem elem
        popIndent
        printLn "}"

    els@(Else _) -> do
        printLn $ "else {"
        pushIndent
        forM_ (elseStmts els) $ \id -> do
            elem <- (Map.! id) . elements <$> gets builder
            cPrettyElem elem
        popIndent
        printLn "}"

    exprStmt@(ExprStmt expr) -> do
        printLn $ show expr ++ ";"

    switch@(Switch _ _) -> do
        printLn $ "switch(" ++ show (switchExpr switch) ++ ") {"
        pushIndent
        forM_ (switchBody switch) $ \id -> do
            elem <- (Map.! id) . elements <$> gets builder
            cPrettyElem elem
        popIndent
        printLn "}"

    cas@(Case _ _) -> do
        printLn $ "case " ++ show (caseExpr cas) ++ ": {"
        pushIndent
        forM_ (caseBody cas) $ \id -> do
            elem <- (Map.! id) . elements <$> gets builder
            cPrettyElem elem
        popIndent
        printLn "}"

    Break -> printLn "break;"

    Set str expr -> do
        printLn $ str ++ " = " ++ show expr ++ ";"


    _ -> error (show elem) 
