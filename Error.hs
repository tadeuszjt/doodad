module Error where

import Control.Monad


data TextPos
    = TextPos { textPos, textLine, textCol :: Int }
    deriving (Eq)


instance Show TextPos where
    show (TextPos p l c) = "(" ++ show p ++ ":" ++ show l ++ ":" ++ show c ++ ")"


newtype CmpError
    = CmpError { getCmpError :: (TextPos, String) }
    deriving (Show)


printError :: CmpError -> String -> IO ()
printError (CmpError (TextPos p l c, str)) source = do
    putStrLn ("error " ++ show l ++ ":" ++ show c ++ " " ++ str ++ ":")
    let sourceLines = lines source
    unless (length sourceLines <= 1) $ putStrLn (sourceLines !! (l-2))
    unless (length sourceLines <= 0) $ putStrLn (sourceLines !! (l-1))
    putStrLn (replicate (c-1) '-' ++ "^")
