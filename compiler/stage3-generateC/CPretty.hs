{-# LANGUAGE FlexibleContexts #-}
module CPretty where

import Data.List
import qualified Data.Map as Map 
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Monad
import CBuilder


printIndent :: BoM Int m => m ()
printIndent = do
    level <- get
    void $ replicateM level $ liftIO $ putStr "\t"

pushIndent :: BoM Int m => m ()
pushIndent = modify (+1)

popIndent :: BoM Int m => m ()
popIndent = modify $ \n -> n - 1



cPretty :: BoM Int m => CBuilderState -> m ()
cPretty state = do
    liftIO $ putStrLn $ "/* Doodad Module: " ++ moduleName state ++ " */"
    liftIO $ putStrLn "#include <math.h>"
    liftIO $ putStrLn "#include <gc.h>"

    let elems = elements state
    let global = elems Map.! (ID 0)
    
    forM_ (globalBody global) $ \id -> do
        case elems Map.! id of
            func@(Func _ _ _ _) -> do
                liftIO $ putStrLn ""
                liftIO $ putStrLn $ show (funcRetty func) ++ " " ++ funcName func ++ "(" ++ intercalate ", " (map show $ funcArgs func) ++ ") {"
                pushIndent
                popIndent
                liftIO $ putStrLn $ "}"

            extern@(Extern _ _ _) -> do
                liftIO $ putStr $ "extern " ++ show (extRetty extern) ++ " " ++ extName extern ++ "("
                forM_ (zip (extArgs extern) [0..]) $ \(argType, i) -> do
                    liftIO $ putStr (show argType)
                    if i < length (extArgs extern) - 1 then liftIO $ putStr ", "
                    else liftIO $ putStr ""
                liftIO $ putStrLn ");"

            typedef@(Typedef _ _) -> do
                liftIO $ putStrLn ""
                liftIO $ putStrLn $ "typedef " ++ show (typedefType typedef) ++ " " ++ typedefName typedef ++ ";"
                    

    liftIO $ putStrLn ""
