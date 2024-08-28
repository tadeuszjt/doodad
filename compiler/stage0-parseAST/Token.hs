module Token where

import Error

data Token
    = Token
        { tokPosn :: TextPos
        , tokType :: TokenType
        , tokStr  :: String
        }
    | TokenImport
        { tokPosn    :: TextPos
        , tokImpExp  :: Bool
        , tokImpPath :: String
        , tokImpAs   :: Maybe String
        }
    deriving (Eq)


instance Show Token where
    show (Token p t s) = show t ++ ": " ++ s
    show (TokenImport p b s _) = "import:" ++ s

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
    | Dedent
    | EmbedC
    | CInclude
    | CLink
    | EOF
    deriving (Show, Eq)
