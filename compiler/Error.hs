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
    | ErrorSrc
        { errSrc  :: String
        , errPos  :: TextPos
        , errStr  :: String
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
    ErrorSrc src pos str   -> printMsg "" src pos str
    ErrorFile path pos str -> do
        file <- try (readFile path) :: IO (Either SomeException String)
        case file of
            Left e    -> putStrLn ("Failed to print error, couldn't read path: " ++ path)
            Right src -> printMsg (path ++ ":") src pos str

    where
        printMsg :: String -> String -> TextPos -> String -> IO ()
        printMsg pre src pos str = do
            let TextPos _ p l c = pos
            putStrLn (pre ++ show l ++ ":" ++ show c ++ ": " ++ str)
            let srcLines = lines src
            unless (l < 3) $ putStrLn (srcLines !! (l-3))
            unless (l < 2) $ putStrLn (srcLines !! (l-2))
            unless (l < 1) $ putStrLn (srcLines !! (l-1))
            when   (l == 0) $ putStrLn (srcLines !! 0)
            putStrLn (replicate (c-1) '-' ++ "^")
