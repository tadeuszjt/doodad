module Lexer where

import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.IO.Temp
import System.IO
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
    withSystemTempFile (takeFileName filename) $ \outFile handle -> do
        cLexFile filename outFile
        lines <- lines <$> hGetContents handle

        forM lines $ \line -> case line of
            _ | isPrefixOf "ident: " line -> do
                return $ Token (TextPos filename 0 0) Ident (fromJust $ stripPrefix "ident: " line)
            _ | isPrefixOf "keyword: " line -> do
                return $ Token (TextPos filename 0 0) Reserved (fromJust $ stripPrefix "keyword: " line)
            _ | isPrefixOf "newline:" line -> do
                return $ Token (TextPos filename 0 0) NewLine ""
            _ | isPrefixOf "indent:" line -> do
                return $ Token (TextPos filename 0 0) Indent ""
            _ | isPrefixOf "dedent:" line -> do
                return $ Token (TextPos filename 0 0) Dedent ""
            _ | isPrefixOf "symbol: " line -> do
                return $ Token (TextPos filename 0 0) TokSym (fromJust $ stripPrefix "symbol: " line)
            _ | isPrefixOf "number: " line -> do
                return $ Token (TextPos filename 0 0) Int (fromJust $ stripPrefix "number: " line)
            _ | isPrefixOf "string: " line -> do
                return $ Token (TextPos filename 0 0) String (fromJust $ stripPrefix "string: " line)
            _ -> error line
