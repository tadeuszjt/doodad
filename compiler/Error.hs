{-# LANGUAGE FlexibleContexts #-}
module Error where

import Control.Monad
import Control.Exception
import Control.Monad.Except 
import Data.Maybe
import qualified Data.Map as Map


data TextPos
    = TextPos
        { textFile :: Int
        , textChar :: Int
        , textLine :: Int
        , textCol  :: Int
        }
    deriving (Eq)


data Error
    = ErrorStr
        { errStr :: String
        }
    | ErrorPos
        { errPos :: TextPos
        , errStr :: String
        }
    | ErrorFile
        { errFile :: FilePath
        , errPos  :: TextPos
        , errStr  :: String
        }
    deriving (Show)


class TextPosition a where
    textPos :: a -> TextPos

instance Show TextPos where
    show (TextPos i p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"

instance TextPosition TextPos where
    textPos = id


withPos :: (MonadError Error m, TextPosition a) => a -> m b -> m b
withPos x f = do
    catchError f $ \e -> case e of
        ErrorStr s   -> throwError $ ErrorPos (textPos x) s
        ErrorPos p s -> throwError $ ErrorPos p s


withFiles :: MonadError Error m => [FilePath] -> m a -> m a
withFiles files f = do
    catchError f $ \e -> case e of
        ErrorStr s   -> throwError e
        ErrorPos p s -> throwError $ ErrorFile (files !! textFile p) p s


assert :: MonadFail m => Bool -> String -> m ()
assert b s = when (not b) (fail s)


printError :: Error -> IO ()
printError err = case err of
    ErrorStr str           -> putStrLn ("error: " ++ str)
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
