module CPretty where

import CGenerate
import qualified Data.Map as Map 
import Control.Monad
import Control.Monad.IO.Class


cPretty :: MonadIO m => CGenerateState -> m ()
cPretty state = do
    let elems = elements state
    let global = elems Map.! (ID 0)
    
    forM_ (globalBody global) $ \id -> do
        case elems Map.! id of
            func@(Func _ _) -> do
                liftIO $ putStrLn $ "void " ++ funcName func ++ " () {"
                liftIO $ putStrLn $ "}"
