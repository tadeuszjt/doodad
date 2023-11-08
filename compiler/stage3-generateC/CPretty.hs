module CPretty where

import Prelude hiding (print)
import Data.List
import qualified Data.Map as Map 
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import System.IO hiding (print)

import Monad
import CBuilder
import CAst


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



print :: String -> DoM CPrettyState ()
print s = do
    printIndent
    handle <- gets fileHandle
    liftIO $ hPutStr handle s

printLn :: String -> DoM CPrettyState ()
printLn s = do
    printIndent
    handle <- gets fileHandle
    liftIO $ hPutStrLn handle s


printIndent :: DoM CPrettyState ()
printIndent = do
    level <- gets indent
    handle <- gets fileHandle
    void $ replicateM level $ liftIO $ hPutStr handle "    "

pushIndent :: DoM CPrettyState ()
pushIndent = modify $ \s -> s { indent = indent s + 1 }

popIndent :: DoM CPrettyState ()
popIndent = modify $ \s -> s { indent = (indent s - 1) }


cPretty :: Set.Set String -> DoM CPrettyState ()
cPretty includePaths = do
    modName <- moduleName <$> gets builder
    printLn $ "/* Doodad Module: " ++ modName ++ " */"
    printLn "#include \"doodad.h\""

    forM_ includePaths $ \includePath -> printLn $ "#include " ++ includePath

    elems <- elements <$> gets builder
    let global = elems Map.! (ID 0)
    forM_ (globalBody global) $ \id -> do
        cPrettyElem (elems Map.! id)

    printLn ""


cPrettyElem :: Element -> DoM CPrettyState ()
cPrettyElem elem = case elem of
    Return expr         -> printLn $ "return " ++ show expr ++ ";"
    ReturnVoid          -> printLn $ "return;"
    Assign typ str expr -> printLn $ show typ ++ " " ++ str ++ " = " ++ show expr ++ ";"
    ExprStmt expr       -> printLn $ show expr ++ ";"
    Break               -> printLn "break;"
    Set expr1 expr2     -> printLn $ show expr1 ++ " = " ++ show expr2 ++ ";"
    Embed str           -> mapM_ printLn (lines str)
    Goto str            -> printLn $ "goto " ++ str ++ ";"
    Label str           -> printLn $ str ++ ":;"

    func@(Func _ _ _ _) -> do
        printLn ""
        printLn $ show (funcRetty func) ++ " " ++ funcName func ++ "(" ++ intercalate ", " (map show $ funcArgs func) ++ ") {"
        pushIndent
        printElems (funcBody func)
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

    if_@(If _ _) -> do
        printLn $ "if (" ++ show (ifExpr if_) ++ ") {"
        pushIndent
        printElems (ifStmts if_)
        popIndent
        printLn "}"

    els@(Else _) -> do
        printLn $ "else {"
        pushIndent
        printElems (elseStmts els)
        popIndent
        printLn "}"

    switch@(Switch _ _) -> do
        printLn $ "switch(" ++ show (switchExpr switch) ++ ") {"
        pushIndent
        printElems (switchBody switch)
        popIndent
        printLn "}"

    cas@(Case _ _) -> do
        printLn $ "case " ++ show (caseExpr cas) ++ ": {"
        pushIndent
        printElems (caseBody cas)
        popIndent
        printLn "}"

    for@(For _ _ _ _) -> do
        printLn ""
        printLn $ "for (" ++ maybe "" show (forInit for) ++ "; " ++ maybe "" show (forCnd for) ++ "; " ++ maybe "" show (forPost for) ++ ") {"
        pushIndent
        printElems (forBody for)
        popIndent
        printLn "}"

    _ -> error (show elem) 
    where 
        printElems :: [ID] -> DoM CPrettyState ()
        printElems ids = do
            forM_ ids $ \id -> do
                elem <- (Map.! id) . elements <$> gets builder
                cPrettyElem elem
