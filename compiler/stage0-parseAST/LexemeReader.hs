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
    liftIO $ system $ "cat " ++ filePath ++ " | ./lexer >  " ++ tempPath

    -- read temp file and delete
    tokens <- mapM makeToken =<< mapM parseLineText =<< (liftIO $ lines <$> readFile tempPath)
    liftIO $ removeFile tempPath

    return tokens

    where 
        parseLineText :: BoM s m => String -> m (Int, Int, String, String)
        parseLineText line = do 
            let int = (read $ takeWhile isDigit line) :: Int
            let textLine = (read $ takeWhile isDigit $ drop 1 $ dropWhile (/= ':') line) :: Int
            let rest = drop 1 $ dropWhile (not . isSpace) (dropWhile isDigit line)
            let code = takeWhile (/= ':') rest
            let rest' = drop 2 $ dropWhile (/= ':') rest
            return (int, textLine, code, rest')


        makeToken :: BoM s m => (Int, Int, String, String) -> m L.Token
        makeToken (textIdx, textLine, code, rest) = do
            let pos = TextPos filePath textLine 0
            case code of 
                "kwd" -> return $ L.Token pos L.Reserved rest
                "typ" -> return $ L.Token pos L.Reserved rest
                "imp" -> return $ L.Token pos L.Import rest
                "idt" -> return $ L.Token pos L.Ident rest
                "sym" -> return $ L.Token pos L.TokSym rest
                "chr" -> return $ L.Token pos L.Char "A"
                "int" -> return $ L.Token pos L.Int rest
                "str" -> return $ L.Token pos L.String rest
                "ind" -> case rest of 
                    "I" -> return $ L.Token pos L.Indent ""
                    "N" -> return $ L.Token pos L.NewLine ""
                    "D" -> return $ L.Token pos L.Dedent ""
                _ -> error (show code)


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