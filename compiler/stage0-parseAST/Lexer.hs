module Lexer where
import Data.Maybe
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import System.FilePath
import System.IO.Temp
import System.IO
import System.Directory
import Token
import Error
import Text.Read (readMaybe)

import Foreign.C.String

foreign import ccall "lexer.h lexFile"
    c_lexFile :: CString -> CString -> IO ()


cLexFile :: String -> String -> IO ()
cLexFile filename filenameOut = do
    withCString filename $ \c_filename ->
        withCString filenameOut $ \c_filenameOut -> c_lexFile c_filename c_filenameOut


lexFile :: Bool -> String -> IO [Token]
lexFile printTokens filename = do
    temp <- writeSystemTempFile (takeFileName filename) ""
    
    cLexFile filename temp
    when printTokens $ putStrLn =<< readFile temp
    lines <- lines <$> readFile temp
    removeFile temp


    tokens <- forM lines $ \line' -> do
        let (lineStr, rest1) = span (/= ':') line'
        let (colStr,  rest2) = span (/= ':') $ drop 1 rest1
        let line = drop 1 rest2
        let pos = TextPos filename (readInt lineStr) (readInt colStr)

        case line of
            l | isPrefixOf "ident: " l -> do
                return $ Token pos Ident (fromJust $ stripPrefix "ident: " l)
            l | isPrefixOf "keyword: " l -> do
                return $ Token pos Reserved (fromJust $ stripPrefix "keyword: " l)
            l | isPrefixOf "newline:" l -> do
                return $ Token pos NewLine ""
            l | isPrefixOf "indent:" l -> do
                return $ Token pos Indent ""
            l | isPrefixOf "dedent:" l -> do
                return $ Token pos Dedent ""
            l | isPrefixOf "symbol: " l -> do
                return $ Token pos TokSym (fromJust $ stripPrefix "symbol: " l)
            l | isPrefixOf "integer: " l -> do
                return $ Token pos Int (fromJust $ stripPrefix "integer: " l)
            l | isPrefixOf "floating: " l -> do
                return $ Token pos Float (fromJust $ stripPrefix "floating: " l)
            l | isPrefixOf "string: " l -> do
                return $ Token pos String (fromJust $ stripPrefix "string: " l)
            l | isPrefixOf "char: " l -> do
                return $ Token pos Char (fromJust $ stripPrefix "char: " l)
            l | isPrefixOf "import: " l -> do
                return $ Token pos Import (dropWhile isSpace $ fromJust $ stripPrefix "import: " l)
            l | isPrefixOf "include: " l -> do
                return $ Token pos CInclude (fromJust $ stripPrefix "include: " l)
            l | isPrefixOf "link: " l -> do
                return $ Token pos CLink (fromJust $ stripPrefix "link: " l)
            l | isPrefixOf "cembed: " l -> do
                return $ Token pos EmbedC (fromJust $ stripPrefix "cembed: " $ replace31 l)
            l -> error ("line is: " ++ l)

    return tokens 
    where
        readInt :: String -> Int
        readInt s = case readMaybe s of
            Nothing -> error "here"
            Just x  -> x


        replace31 :: String -> String
        replace31 (x : xs)
            | ord x == 31 = '\n' : (replace31 xs)
            | otherwise   = x : (replace31 xs)
        replace31 xs = xs
