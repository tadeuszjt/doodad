{
module Lexer
	( Token(..)
	, AlexPosn(..)
	, alexScanTokens
	, tokPosn
	)
where
}

%wrapper "posn"

$white  = [\ \t\n]
$digit  = 0-9
$alpha  = [a-zA-Z]
$symbol = [\{\}\(\)\,\;]

@reserved   = fn | for | if | else | return | print | int | float | bool
@reservedOp = [\+\-\*\/\%\<\>\=] | ":=" | "==" | "<=" | ">=" | "||" | "&&"

tokens :-
	$white                     ; 
	$symbol                    { \p s -> Sym p (head s) }
	@reserved                  { \p s -> Reserved p s }
	@reservedOp                { \p s -> ReservedOp p s }
	$alpha [$alpha $digit \_]* { \p s -> Ident p s }
	$digit+                    { \p s -> Int p (read s) }


{
data Token
	= Sym        AlexPosn Char
	| Reserved   AlexPosn String
	| ReservedOp AlexPosn String
	| Ident      AlexPosn String
	| Int        AlexPosn Int
	deriving (Show, Eq)


tokPosn :: Token -> AlexPosn
tokPosn tok = case tok of
	(Sym p _)        -> p
	(Reserved p _)   -> p
	(ReservedOp p _) -> p
	(Ident p _)      -> p
	(Int p _)        -> p
}
