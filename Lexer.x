{
module Lexer
    ( Token(..)
    , TokenType(..)
    , AlexPosn(..)
    , alexScanner
    , alexMonadScan
    )
where

import Error
}

%wrapper "monad"

$white   = [\ \t\n]
$digit   = 0-9
$alpha   = [a-zA-Z]
$ascsym  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$special = [\(\)\,\;\[\]\`\{\}]
$graphic = [$alpha $digit $ascsym $special \:\"\']

$symbol  = [\{\}\(\)\[\]\,\|\.\;\:\_]

@types      = i64 | i32 | i16 | f64 | f32 | bool | char | string
@builtin    = print | len | append
@keywords   = fn | extern | type | let | while | if | else | return | switch | true | false | module | imports
@reserved   = @keywords | @types | @builtin
@reservedOp = [\+\-\*\/\%\<\>\=] | ":=" | "==" | "<=" | ">=" | "||" | "&&"

@escape     = \\ [tn]
@string     = $graphic # [\"\\] | " " | @escape
@char       = $graphic # [\'\\] | " " | @escape

tokens :-
    $white                           ; 
    $symbol                          { mkT Sym }
    @reserved                        { mkT Reserved }
    @reservedOp                      { mkT ReservedOp }
    $alpha [$alpha $digit \_]*       { mkT Ident }
    $digit+                          { mkT Int }
    $digit+ \. $digit+               { mkT Float }
    \' @char \'                      { mkT Char }
    \" @string* \"                   { mkT String }
{

mkT :: TokenType -> AlexInput -> Int -> Alex (AlexPosn, TokenType, String)
mkT String (p,_,_,s) len = return (p, String, drop 1 (take (len-1) s))
mkT t      (p,_,_,s) len = return (p, t, take len s)

alexEOF = return (undefined, EOF, "")

alexScanner filename str = runAlex str loop
    where
        loop = do
            (p, typ, str) <- alexMonadScan
            if typ == EOF
                then return []
                else do
                    let AlexPn pos line col = p
                    let textPos = TextPos filename pos line col
                    fmap (Token textPos typ str :) loop

data Token
    = Token
        { tokPosn :: TextPos
        , tokType :: TokenType
        , tokStr  :: String
        }
    deriving (Show, Eq)

data TokenType
    = Sym
    | Reserved
    | ReservedOp
    | Ident
    | Int
    | Float
    | Char
    | String
    | EOF
    deriving (Show, Eq)
}
