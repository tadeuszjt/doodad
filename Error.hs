module Error where

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map


data TextPos
    = TextPos
        { textFile :: String
        , textPos  :: Int
        , textLine :: Int
        , textCol  :: Int
        }
    deriving (Eq)


instance Show TextPos where
    show (TextPos f p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


data Error
    = ErrorStr
        { errStr :: String
        }
    | ErrorFile
        { errPos  :: TextPos
        , errStr  :: String
        }
    deriving (Show)


printError :: Error -> Map.Map String String -> IO ()
printError err srcFiles = case err of
    ErrorStr str      -> putStrLn ("error: " ++ str)
    ErrorFile pos str -> do
        let TextPos filename p l c = pos
        let srcm = Map.lookup filename srcFiles
        case srcm of
            Nothing  -> putStrLn ("error (no source): " ++ str)
            Just src -> do
                let sourceLines   = lines src
                putStrLn ("error " ++ show pos ++ " " ++ str ++ ":")
                unless (l < 2) $ putStrLn (sourceLines !! (l-2))
                unless (l < 1) $ putStrLn (sourceLines !! (l-1))
                putStrLn (replicate (c-1) '-' ++ "^")
