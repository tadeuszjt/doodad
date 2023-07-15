module Lexer where

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

import Foreign.C.String

foreign import ccall "lexer.h lexFile"
    c_lexFile :: CString -> CString -> IO ()


cLexFile :: String -> String -> IO ()
cLexFile filename filenameOut = do
    withCString filename $ \c_filename ->
        withCString filenameOut $ \c_filenameOut -> c_lexFile c_filename c_filenameOut


lexFile :: String -> IO [Token]
lexFile filename = do
    temp <- writeSystemTempFile (takeFileName filename) ""
    
    cLexFile filename temp
    --putStrLn =<< readFile temp
    lines <- lines <$> readFile temp
    removeFile temp

    let pos = TextPos filename 0 0

    tokens <- forM lines $ \line -> case line of
        _ | isPrefixOf "ident: " line -> do
            return $ Token pos Ident (fromJust $ stripPrefix "ident: " line)
        _ | isPrefixOf "keyword: " line -> do
            return $ Token pos Reserved (fromJust $ stripPrefix "keyword: " line)
        _ | isPrefixOf "newline:" line -> do
            return $ Token pos NewLine ""
        _ | isPrefixOf "indent:" line -> do
            return $ Token pos Indent ""
        _ | isPrefixOf "dedent:" line -> do
            return $ Token pos Dedent ""
        _ | isPrefixOf "symbol: " line -> do
            return $ Token pos TokSym (fromJust $ stripPrefix "symbol: " line)
        _ | isPrefixOf "number: " line -> do
            return $ Token pos Int (fromJust $ stripPrefix "number: " line)
        _ | isPrefixOf "string: " line -> do
            return $ Token pos String (fromJust $ stripPrefix "string: " line)
        _ | isPrefixOf "char: " line -> do
            return $ Token pos Char (fromJust $ stripPrefix "char: " line)
        _ | isPrefixOf "import: " line -> do
            return $ Token pos Import (fromJust $ stripPrefix "import: " line)
        _ | isPrefixOf "embed_c: " line -> do
            return $ Token pos EmbedC (fromJust $ stripPrefix "embed_c: " $ replace31 line)
        _ -> error line

    return tokens 
    where
        replace31 :: String -> String
        replace31 (x : xs)
            | ord x == 31 = '\n' : (replace31 xs)
            | otherwise   = x : (replace31 xs)
        replace31 xs = xs
