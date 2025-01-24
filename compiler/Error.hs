{-# LANGUAGE FlexibleContexts #-}
module Error where

import Control.Monad
import Control.Exception
import Control.Monad.Except
import qualified Data.Map as Map


data TextPos
    = TextPos
        { textFile :: FilePath
        , textLine :: Int
        , textCol  :: Int
        }
    deriving (Eq, Ord)


data Error
    = ErrorStr
        { errStr :: String
        }
    | ErrorPos
        { errPos :: TextPos
        , errStr :: String
        }
    deriving ()


class TextPosition a where textPos :: a -> TextPos
instance TextPosition TextPos where textPos = id
instance Show TextPos where show (TextPos f l c) = f ++ ":" ++ show l ++ ":" ++ show c
instance Show Error where show (ErrorStr s) = s


withPos :: (MonadError Error m, TextPosition a) => a -> m b -> m b
withPos x f = do
    catchError f $ \e -> case e of
        ErrorStr s   -> throwError $ ErrorPos (textPos x) s
        ErrorPos p s -> throwError $ ErrorPos p s


withErrorPrefix :: MonadError Error m => String -> m a -> m a
withErrorPrefix str f = do
    catchError f $ \e -> case e of
        ErrorStr s         -> throwError $ ErrorStr (str ++ s)
        ErrorPos p s       -> throwError $ ErrorPos p (str ++ s)

check :: MonadFail m => Bool -> String -> m ()
check b s = unless b (fail s)


mapGet :: (MonadFail m, Ord k, Show k) => k -> Map.Map k v -> m v
mapGet key map = do
    case Map.lookup key map of
        Nothing -> fail $ show key ++ " is not a member of map"
        Just x  -> return x


printError :: Error -> IO ()
printError err = case err of
    ErrorStr str           -> putStrLn ("error: " ++ str)
    ErrorPos pos str       -> do
        let path = textFile pos
        file <- try (readFile path) :: IO (Either SomeException String)
        case file of
            Left e    -> do
                putStrLn ("Error: couldn't read path: " ++ show path)
                putStrLn str
            Right src -> printMsg src pos str
    where
        printMsg :: String -> TextPos -> String -> IO ()
        printMsg src pos str = do
            putStrLn (show pos ++ " " ++ str)
            let srcLines = lines src
            unless (textLine pos < 3) $ putStrLn (srcLines !! (textLine pos - 3))
            unless (textLine pos < 2) $ putStrLn (srcLines !! (textLine pos - 2))
            unless (textLine pos < 1) $ putStrLn (srcLines !! (textLine pos - 1))
            --when   (l == 0) $ putStrLn (srcLines !! 0)
            putStrLn (replicate (textCol pos - 1) '-' ++ "^")
