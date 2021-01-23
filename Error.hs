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


printError :: Error -> IO ()
printError err = case err of
    ErrorStr str      -> putStrLn ("error: " ++ str)
    ErrorFile pos str -> do
        let TextPos path p l c = pos
        sourceLines <- fmap lines (readFile path)
        
        putStrLn ("error " ++ show pos ++ " " ++ str ++ ":")
        unless (l < 3) $ putStrLn (sourceLines !! (l-3))
        unless (l < 2) $ putStrLn (sourceLines !! (l-2))
        unless (l < 1) $ putStrLn (sourceLines !! (l-1))
        putStrLn (replicate (c-1) '-' ++ "^")
