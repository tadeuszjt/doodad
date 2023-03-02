module LexemeReader where

import Control.Monad
import Text.Read
import Data.Char
import System.IO
import System.Directory
import System.IO.Temp
import System.Cmd
import Control.Monad.IO.Class
import Lexer as L

import Error
import Monad

--
--data Token
--    = Token
--        { tokPosn :: TextPos
--        , tokType :: TokenType
--        , tokStr  :: String
--        }
--    deriving (Eq)
--
--
--instance Show Token where
--    show (Token p t s) = show t ++ ": " ++ s
--


lexFile :: BoM s m => FilePath -> m [L.Token]
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


        makeToken :: BoM s m => (Int, Int, Int, String, String) -> m L.Token
        makeToken (textIdx, textLine, textCol, code, rest) = do
            let pos = TextPos filePath textLine textCol
            case code of 
                "kwd" -> return $ L.Token pos L.Reserved rest
                "typ" -> return $ L.Token pos L.Reserved rest
                "imp" -> return $ L.Token pos L.Import rest
                "imc" -> return $ L.Token pos L.ImportC rest
                "idt" -> return $ L.Token pos L.Ident rest
                "sym" -> return $ L.Token pos L.TokSym rest
                "chr" -> return $ L.Token pos L.Char rest
                "int" -> return $ L.Token pos L.Int rest
                "flt" -> return $ L.Token pos L.Float rest
                "str" -> return $ L.Token pos L.String rest
                "ind" -> case rest of 
                    "I" -> return $ L.Token pos L.Indent ""
                    "N" -> return $ L.Token pos L.NewLine ""
                    "D" -> return $ L.Token pos L.Dedent ""
                _ -> error (show code ++ " " ++ show rest ++ show filePath)


--
--data TokenType
--    = TokSym
--    | Reserved
--    | ReservedOp
--    | Ident
--    | Int
--    | Float
--    | Char
--    | String
--    | Indent
--    | NewLine
--    | Import
--    | ImportC
--    | ImportCMacro
--    | Dedent
--    | EOF
--    deriving (Show, Eq)
