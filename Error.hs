module Error where

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map


data TextPos
    = TextPos { textPos, textLine, textCol :: Int }
    deriving (Eq)


instance Show TextPos where
    show (TextPos p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


data Error
    = ErrorStr
        { errStr :: String
        }
    | ErrorFile
        { errFile :: String
        , errPos  :: TextPos
        , errStr  :: String
        }
    deriving (Show)


printError :: Error -> Map.Map String String -> IO ()
printError err srcFiles = case err of
    ErrorStr str        -> putStrLn ("error: " ++ str)
    ErrorFile f pos str -> do
        let srcm = Map.lookup f srcFiles
        case srcm of
            Nothing  -> putStrLn ("error (no source): " ++ str)
            Just src -> do
                let sourceLines   = lines src
                let TextPos p l c = pos
                putStrLn ("error " ++ show pos ++ " " ++ str ++ ":")
                unless (l < 2) $ putStrLn (sourceLines !! (l-2))
                unless (l < 1) $ putStrLn (sourceLines !! (l-1))
                putStrLn (replicate (c-1) '-' ++ "^")
