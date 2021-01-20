{
module Lexer
    ( Token(..)
    , TokenType(..)
    , AlexPosn(..)
    , alexScanner
    , alexMonadScan
    )
where

import Data.List
import Data.Maybe
import Error
}

%wrapper "monadUserState"

$white   = [\ ]
$newline = \n
$tab     = \t
$digit   = 0-9
$alpha   = [a-zA-Z]
$ascsym  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$special = [\(\)\,\;\[\]\`\{\}]
$graphic = [$alpha $digit $ascsym $special \:\"\']

$symbol  = [\{\}\(\)\[\]\,\|\.\;\:\_]

@types      = i16 | i32 | i64 | f32 | f64 | bool | char | string
@builtin    = print | len | append
@keywords   = fn | extern | type | let | while | if | else | return | switch | true | false | module | imports
@reserved   = @keywords | @types | @builtin
@reservedOp = [\+\-\*\/\%\<\>\=] | ":=" | "==" | "<=" | ">=" | "||" | "&&"

@string     = $graphic # [\"\\] | " " 
@char       = $graphic # [\'\\] | " "

tokens :-
    $white                                          { mkT NoToken }
    $symbol                                         { mkT Sym }
    @reserved                                       { mkT Reserved }
    @reservedOp                                     { mkT ReservedOp }
    $alpha [$alpha $digit \_]*                      { mkT Ident }
    $digit+                                         { mkT Int }
    $digit+ \. $digit+                              { mkT Float }
    \' @char \'                                     { mkT Char }
    \" @string* \"                                  { mkT String }
    \' \\ n \'                                      { mkT Char }
    [$newline $tab $white]* $newline [$tab $white]* { mkIndentT }
{

mkT :: TokenType -> AlexInput -> Int -> Alex (AlexPosn, TokenType, String)
mkT String (p,_,_,s) len = return (p, String, drop 1 (take (len-1) s))
mkT t      (p,_,_,s) len = return (p, t, take len s)


mkIndentT :: AlexInput -> Int -> Alex (AlexPosn, TokenType, String)
mkIndentT (p,_,_,s) len = do
    let lineIndent = reverse $ takeWhile (/= '\n') $ reverse (take len s)
    curIndent <- fmap (concat . reverse)  getLexerIndentStack

    if lineIndent == curIndent then
        return (p, NewLine, "")
    else if curIndent `isPrefixOf` lineIndent then do
        let s = fromJust (stripPrefix curIndent lineIndent)
        pushIndent s
        return (p, Indent, "")
    else if lineIndent `isPrefixOf` curIndent then
        return (p, Dedent, lineIndent)
    else
        error ("invalid indentation of: " ++ map rep lineIndent)

    where
        rep c = case c of
            '\t' -> 't'
            ' '  -> 's'
            _    -> c


alexEOF = return (undefined, EOF, "")

alexScanner filename str = runAlex str loop
    where
        loop = do
            (p, typ, str) <- alexMonadScan
            let AlexPn pos line col = p
            let textPos = TextPos filename pos line col
            case typ of
                NoToken -> loop
                EOF     -> return []
                Dedent  -> do
                    toks <- dedentLoop textPos str
                    fmap (toks ++) loop

                _       -> fmap (Token textPos typ str :) loop
            
        dedentLoop textPos indent = do
            curIndent <- fmap concat getLexerIndentStack
            if (length indent) < (length curIndent)
            then do
                popIndent
                fmap (Token textPos Dedent "" :) (dedentLoop textPos indent)
            else return []
                


data AlexUserState
    = AlexUserState
        { lexerIndentStack :: [String]
        }

alexInitUserState =
    AlexUserState
        { lexerIndentStack = [""]
        }

getLexerIndentStack :: Alex [String]
getLexerIndentStack = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerIndentStack ust)

setLexerIndentStack :: [String] -> Alex ()
setLexerIndentStack i = Alex $ \s -> Right (s { alex_ust = (alex_ust s){lexerIndentStack = i}}, ())


pushIndent :: String -> Alex ()
pushIndent s = do
    stack <- getLexerIndentStack
    setLexerIndentStack (s:stack)


popIndent :: Alex String
popIndent = do
    stack <- getLexerIndentStack
    setLexerIndentStack (tail stack)
    return (head stack)


data Token
    = Token
        { tokPosn :: TextPos
        , tokType :: TokenType
        , tokStr  :: String
        }
    deriving (Eq)


instance Show Token where
    show (Token p t s) = show t ++ ":" ++ s

data TokenType
    = Sym
    | Reserved
    | ReservedOp
    | Ident
    | Int
    | Float
    | Char
    | String
    | Indent
    | NewLine
    | Dedent
    | NoToken
    | EOF
    deriving (Show, Eq)
}
