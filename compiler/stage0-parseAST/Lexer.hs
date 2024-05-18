module Lexer where
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import System.FilePath
import System.IO.Temp
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

    forM lines $ \line' -> do
        let (lineStr, rest1) = span (/= ':') line'
        let (colStr,  rest2) = span (/= ':') $ drop 1 rest1
        let line = drop 1 rest2
        let pos = TextPos filename (readInt lineStr) (readInt colStr)

        return $ case line of
            l | isPrefixOf "ident: " l    -> Token pos Ident (fromJust $ stripPrefix "ident: " l)
            l | isPrefixOf "Ident: " l    -> Token pos UpperIdent (fromJust $ stripPrefix "Ident: " l)
            l | isPrefixOf "keyword: " l  -> Token pos Reserved (fromJust $ stripPrefix "keyword: " l)
            l | isPrefixOf "newline:" l   -> Token pos NewLine ""
            l | isPrefixOf "indent:" l    -> Token pos Indent ""
            l | isPrefixOf "dedent:" l    -> Token pos Dedent ""
            l | isPrefixOf "symbol: " l   -> Token pos TokSym (fromJust $ stripPrefix "symbol: " l)
            l | isPrefixOf "integer: " l  -> Token pos Int (fromJust $ stripPrefix "integer: " l)
            l | isPrefixOf "floating: " l -> Token pos Float (fromJust $ stripPrefix "floating: " l)
            l | isPrefixOf "string: " l   -> Token pos String (fromJust $ stripPrefix "string: " l)
            l | isPrefixOf "char: " l     -> Token pos Char (fromJust $ stripPrefix "char: " l)
            l | isPrefixOf "import: " l   -> Token pos Import (dropWhile isSpace $ fromJust $ stripPrefix "import: " l)
            l | isPrefixOf "include: " l  -> Token pos CInclude (fromJust $ stripPrefix "include: " l)
            l | isPrefixOf "module: " l   -> Token pos Module (fromJust $ stripPrefix "module: " l)
            l | isPrefixOf "link: " l     -> Token pos CLink (fromJust $ stripPrefix "link: " l)
            l | isPrefixOf "cembed: " l   -> Token pos EmbedC (fromJust $ stripPrefix "cembed: " $ replace31 l)
            l -> error ("line is: " ++ l)
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
