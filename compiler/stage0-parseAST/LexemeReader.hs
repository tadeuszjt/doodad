module LexemeReader where

import Control.Monad
import Text.Read
import Data.Char
import System.IO
import System.Directory
import System.IO.Temp
import System.Cmd
import Control.Monad.IO.Class

import Error
import Monad

data Token
    = Token
        { tokPosn :: TextPos
        , tokType :: TokenType
        , tokStr  :: String
        }
    deriving (Eq)


instance Show Token where
    show (Token p t s) = show t ++ ": " ++ s

data TokenType
    = TokSym
    | Reserved
    | Ident
    | Int
    | Float
    | Char
    | String
    | Indent
    | NewLine
    | Import
    | ImportC
    | ImportCMacro
    | Dedent
    | EOF
    deriving (Show, Eq)
 

lexFile :: BoM s m => FilePath -> m [Token]
lexFile filePath = do 
    src <- liftIO $ lines <$> readFile filePath

    -- create temp file, fill with result of lexer
    (tempPath, tempHandle) <- liftIO $ openTempFile "." "temp"
    liftIO $ hClose tempHandle
    liftIO $ system $ "cat " ++ filePath ++ " | ./bin/lexer >  " ++ tempPath

    -- read temp file and delete
    tokens <- mapM makeToken =<< mapM parseLineText =<< (liftIO $ lines <$> readFile tempPath)
    liftIO $ removeFile tempPath

    return tokens

    where 
        parseLineText :: BoM s m => String -> m (Int, Int, Int, String, String)
        parseLineText line = do 
            let pos = (read $ takeWhile isDigit line) :: Int
            let lineLine = drop 1 $ dropWhile (/= ':') line
            let lineNum = (read $ takeWhile isDigit lineLine) :: Int 
            let colLine = drop 1 $ dropWhile (/= ':') lineLine
            let col = (read $ takeWhile isDigit colLine) :: Int 

            let code = takeWhile (/= ':') $ drop 1 $ dropWhile (/= '\t') line 
            let rest = drop 2 $ dropWhile (/= ':') $ drop 1 $ dropWhile (/= '\t') line 
            return (pos, lineNum, col, code, rest)


        makeToken :: BoM s m => (Int, Int, Int, String, String) -> m Token
        makeToken (textIdx, textLine, textCol, code, rest) = do
            let pos = TextPos filePath textLine textCol
            case code of 
                "kwd" -> return $ Token pos Reserved rest
                "typ" -> return $ Token pos Reserved rest
                "imp" -> return $ Token pos Import rest
                "imc" -> return $ Token pos ImportC rest
                "idt" -> return $ Token pos LexemeReader.Ident rest
                "sym" -> return $ Token pos TokSym rest
                "chr" -> return $ Token pos LexemeReader.Char rest
                "int" -> return $ Token pos Int rest
                "flt" -> return $ Token pos Float rest
                "str" -> return $ Token pos LexemeReader.String rest
                "ind" -> case rest of 
                    "I" -> return $ Token pos Indent ""
                    "N" -> return $ Token pos NewLine ""
                    "D" -> return $ Token pos Dedent ""
                _ -> error (show code ++ " " ++ show rest ++ show filePath)
