module Token where

import Error

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
    | UpperIdent
    | Int
    | Float
    | Char
    | String
    | Indent
    | NewLine
    | Module
    | Import
    | Dedent
    | EmbedC
    | CInclude
    | CLink
    | EOF
    deriving (Show, Eq)
