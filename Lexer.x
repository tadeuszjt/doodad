{
module Lexer
	( Token(..)
	, TokenType(..)
	, AlexPosn(..)
	, alexScanner
	, alexMonadScan
	)
where
}

%wrapper "monad"

$white  = [\ \t\n]
$digit  = 0-9
$alpha  = [a-zA-Z]
$symbol = [\{\}\(\)\,\;]

@reserved   = fn | for | if | else | return | print | int | float | bool
@reservedOp = [\+\-\*\/\%\<\>\=] | ":=" | "==" | "<=" | ">=" | "||" | "&&"

tokens :-
	$white                     ; 
	$symbol                    { mkT Sym }
	@reserved                  { mkT Reserved }
	@reservedOp                { mkT ReservedOp }
	$alpha [$alpha $digit \_]* { mkT Ident }
	$digit+                    { mkT Int }


{

mkT :: TokenType -> AlexInput -> Int -> Alex Token
mkT t (p,_,_,s) len = return $ Token p t (take len s)

alexEOF = return (Token undefined EOF "")

alexScanner str = runAlex str (loop)
	where
		loop = do
			tok@(Token _ t _) <- alexMonadScan
			if t == EOF
				then return []
				else do
					ts <- loop
					return (tok:ts)


data Token
	= Token
		{ tokPosn :: AlexPosn
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
	| EOF
	deriving (Show, Eq)
}
