module Error where

import Control.Monad
import Control.Exception
import Data.Maybe
import qualified Data.Map as Map


data TextPos
    = TextPos
        { textFile :: Int
        , textPos  :: Int
        , textLine :: Int
        , textCol  :: Int
        }
    deriving (Eq)


instance Show TextPos where
    show (TextPos i p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


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


printError :: Error -> IO ()
printError err = case err of
    ErrorStr str           -> putStrLn ("error: " ++ str)
    ErrorFile path pos str -> do

        let TextPos i p l c = pos
        putStrLn ("error " ++ show pos ++ " " ++ str ++ ":")

        file <- try (readFile path) :: IO (Either SomeException String)

        case file of
            Left e -> putStrLn ("couldn't read: " ++ path)
            Right source -> do
                let sourceLines = lines source
                unless (l < 3) $ putStrLn (sourceLines !! (l-3))
                unless (l < 2) $ putStrLn (sourceLines !! (l-2))
                unless (l < 1) $ putStrLn (sourceLines !! (l-1))
                putStrLn (replicate (c-1) '-' ++ "^")
