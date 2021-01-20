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

@types      = i64 | i32 | i16 | f64 | f32 | bool | char | string
@builtin    = print | len | append
@keywords   = fn | extern | type | let | while | if | else | return | switch | true | false | module | imports
@reserved   = @keywords | @types | @builtin
@reservedOp = [\+\-\*\/\%\<\>\=] | ":=" | "==" | "<=" | ">=" | "||" | "&&"

@string     = $graphic # [\"\\] | " " 
@char       = $graphic # [\'\\] | " "

tokens :-
    $white                                 ; 
    $symbol                                { mkT Sym }
    @reserved                              { mkT Reserved }
    @reservedOp                            { mkT ReservedOp }
    $alpha [$alpha $digit \_]*             { mkT Ident }
    $digit+                                { mkT Int }
    $digit+ \. $digit+                     { mkT Float }
    \' @char \'                            { mkT Char }
    \" @string* \"                         { mkT String }
    \' \\ n \'                             { mkT Char }
    [$newline $tab $white]* $newline $tab* { mkIndentT }
{

mkT :: TokenType -> AlexInput -> Int -> Alex (AlexPosn, TokenType, String)
mkT String (p,_,_,s) len = return (p, String, drop 1 (take (len-1) s))
mkT t      (p,_,_,s) len = return (p, t, take len s)


mkIndentT :: AlexInput -> Int -> Alex (AlexPosn, TokenType, String)
mkIndentT (p,_,_,s) len = do
    let str = take len s
    let lineLevel = length $ takeWhile (== '\t') (reverse str)
    stack <- getLexerIndentLevel
    let curLevel  = head stack

    if lineLevel > curLevel then do
        pushIndent lineLevel
        return (p, Indent, "")

    else if lineLevel == curLevel then do
        return (p, NewLine, "")

    else
        return (p, Dedent, show lineLevel)


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
                    toks <- dedentLoop textPos (read str :: Int)
                    fmap (toks ++) loop

                _       -> fmap (Token textPos typ str :) loop
            
        dedentLoop textPos level = do
            i <- popIndent
            if level < i
            then fmap (Token textPos Dedent "" :) (dedentLoop textPos level)
            else pushIndent i >> return []
                


data AlexUserState
    = AlexUserState
        { lexerIndentLevel :: [Int]
        }

alexInitUserState =
    AlexUserState
        { lexerIndentLevel = [0]
        }

getLexerIndentLevel :: Alex [Int]
getLexerIndentLevel = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerIndentLevel ust)

setLexerIndentLevel :: [Int] -> Alex ()
setLexerIndentLevel i = Alex $ \s -> Right (s { alex_ust = (alex_ust s){lexerIndentLevel = i}}, ())


pushIndent :: Int -> Alex ()
pushIndent i = do
    stack <- getLexerIndentLevel
    setLexerIndentLevel (i:stack)


popIndent :: Alex Int
popIndent = do
    stack <- getLexerIndentLevel
    setLexerIndentLevel (tail stack)
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
