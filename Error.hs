module Error where

import Control.Monad
import Data.Maybe


data TextPos
    = TextPos { textPos, textLine, textCol :: Int }
    deriving (Eq)


instance Show TextPos where
    show (TextPos p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


newtype CmpError
    = CmpError { getCmpError :: (Maybe TextPos, String) }
    deriving (Show)


printError :: CmpError -> String -> IO ()
printError (CmpError (Nothing, str)) source = putStrLn ("error: " ++ str)
printError (CmpError (Just pos@(TextPos p l c), str)) source = do
    putStrLn ("error " ++ show pos ++ " " ++ str ++ ":")
    let sourceLines = lines source
    unless (l < 2) $ putStrLn (sourceLines !! (l-2))
    unless (l < 1) $ putStrLn (sourceLines !! (l-1))
    putStrLn (replicate (c-1) '-' ++ "^")
