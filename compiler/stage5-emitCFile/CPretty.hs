{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CPretty where

import Prelude hiding (print)
import Data.List
import qualified Data.Map as Map 
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO hiding (print)

import CBuilder
import CAst


newtype CPretty a = CPretty
    { unCPretty :: ReaderT (BuilderState, Handle) (StateT Int IO) a }
    deriving (Functor, Applicative, Monad, MonadReader (BuilderState, Handle), MonadState Int, MonadIO)


runCPretty :: MonadIO m => BuilderState -> Handle -> CPretty a -> m a
runCPretty builderState handle f =
    fmap fst $ liftIO $ runStateT (runReaderT (unCPretty f) (builderState, handle)) 0


print :: String -> CPretty ()
print s = do
    printIndent
    handle <- snd <$> ask
    liftIO (hPutStr handle s)

printLn :: String -> CPretty ()
printLn s = do
    printIndent
    handle <- snd <$> ask 
    liftIO (hPutStrLn handle s)


printIndent :: CPretty ()
printIndent = do
    level <- get
    handle <- snd <$> ask
    void $ replicateM level $ liftIO $ hPutStr handle "    "

pushIndent :: CPretty ()
pushIndent = modify (+1)

popIndent :: CPretty ()
popIndent = modify $ \s -> s - 1


cPretty :: Set.Set String -> CPretty ()
cPretty includePaths = do
    printLn "#include \"doodad.h\""

    forM_ includePaths $ \includePath -> printLn $ "#include " ++ includePath

    typeDefs <- typeDefs . fst <$> ask
    mapM_ cPrettyElem (reverse typeDefs)

    externs <- externs . fst <$> ask
    mapM_ cPrettyElem (reverse externs)

    elems <- elements . fst <$> ask
    let global = elems Map.! (ID 0)
    forM_ (globalBody global) $ \id -> do
        cPrettyElem (elems Map.! id)

    printLn ""


cPrettyElem :: Element -> CPretty ()
cPrettyElem elem = case elem of
    Return expr         -> printLn $ "return " ++ showNoParens expr ++ ";"
    ReturnVoid          -> printLn $ "return;"

    Assign typ str expr -> do
        case typ of
            Carray n t -> printLn $ show t ++ " " ++ str ++ "[" ++ show n ++ "]" ++ " = " ++ showNoParens expr ++ ";"
            _          -> printLn $ show typ ++ " " ++ str ++ " = " ++ showNoParens expr ++ ";"

    ExprStmt expr       -> printLn $ showNoParens expr ++ ";"
    Break               -> printLn "break;"
    Set expr1 expr2     -> printLn $ showNoParens expr1 ++ " = " ++ showNoParens expr2 ++ ";"
    Embed str           -> mapM_ printLn (lines str)
    Goto str            -> printLn $ "goto " ++ str ++ ";"
    Label str           -> printLn $ str ++ ":;"

    func@(Func _ _ _ _ _) -> do
        printLn ""
        forM (funcQualifiers func) $ \qualifier ->
            print (show qualifier ++ " ")
        printLn $ show (funcRetty func) ++ " " ++ funcName func ++ "(" ++ intercalate ", " (map show $ funcArgs func) ++ ") {"
        pushIndent
        printElems (funcBody func)
        popIndent
        printLn $ "}"

    extern@(ExternFunc _ _ _ _) -> do
        forM (extQualifiers extern) $ \qualifier -> print (show qualifier ++ " ")
        print $ show (extRetty extern) ++ " " ++ extName extern ++ "("
        forM_ (zip (extArgs extern) [0..]) $ \(argType, i) -> do
            print (show argType)
            if i < length (extArgs extern) - 1 then print ", "
            else print ""
        printLn ");"

    typedef@(Typedef _ _) -> do
        printLn ""
        printLn $ "typedef " ++ show (typedefType typedef) ++ " " ++ typedefName typedef ++ ";"

    if_@(If _ _) -> do
        printLn $ "if (" ++ showNoParens (ifExpr if_) ++ ") {"
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
        printLn $ "switch(" ++ showNoParens (switchExpr switch) ++ ") {"
        pushIndent
        printElems (switchBody switch)
        popIndent
        printLn "}"

    cas@(Case _ _) -> do
        printLn $ "case " ++ showNoParens (caseExpr cas) ++ ": {"
        pushIndent
        printElems (caseBody cas)
        popIndent
        printLn "}"

    for@(For _ _ _ _) -> do
        printLn ""
        printLn $ "for (" ++ maybe "" showNoParens (forInit for) ++ "; " ++ maybe "" showNoParens (forCnd for) ++ "; " ++ maybe "" show (forPost for) ++ ") {"
        pushIndent
        printElems (forBody for)
        popIndent
        printLn "}"

    _ -> error (show elem) 
    where 
        printElems :: [ID] -> CPretty ()
        printElems ids = do
            forM_ ids $ \id -> do
                elem <- (Map.! id) . elements . fst <$> ask
                cPrettyElem elem
