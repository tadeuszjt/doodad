{
module Parser where
import qualified Lexer as L
import qualified AST as S
}

%name      parseTokens 
%tokentype { L.Token }
%monad { P } { thenP } { returnP }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  '<' '>'
%nonassoc  '<=' '>='
%nonassoc  '(' ')'


%token
    '='        { L.Token _ L.ReservedOp "=" }
    '+'        { L.Token _ L.ReservedOp "+" }
    '-'        { L.Token _ L.ReservedOp "-" }
    '*'        { L.Token _ L.ReservedOp "*" }
    '/'        { L.Token _ L.ReservedOp "/" }
    '%'        { L.Token _ L.ReservedOp "%" }
    '<'        { L.Token _ L.ReservedOp "<" }
    '>'        { L.Token _ L.ReservedOp ">" }
    ':='       { L.Token _ L.ReservedOp ":=" }
    '<='       { L.Token _ L.ReservedOp "<=" }
    '>='       { L.Token _ L.ReservedOp ">=" }
    '=='       { L.Token _ L.ReservedOp "==" }
    '&&'       { L.Token _ L.ReservedOp "&&" }
    '||'       { L.Token _ L.ReservedOp "||" }

    fn         { L.Token _ L.Reserved "fn" }
    if         { L.Token _ L.Reserved "if" }
    else       { L.Token _ L.Reserved "else" }
    for        { L.Token _ L.Reserved "for" }
    return     { L.Token _ L.Reserved "return" }
    print      { L.Token _ L.Reserved "print" }

    int        { L.Token _ L.Int _ }
    ident      { L.Token _ L.Ident _ }

    '('        { L.Token _ L.Sym "(" }
    ')'        { L.Token _ L.Sym ")" }
    '{'        { L.Token _ L.Sym "{" }
    '}'        { L.Token _ L.Sym "}" }
    ','        { L.Token _ L.Sym "," }
    ';'        { L.Token _ L.Sym ";" }

%%

Prog : Stmt ';'           { [$1] }
	 | Stmt ';' Prog      { $1 : $3 }

Stmt : ident ':=' Expr    { S.Assign (L.tokPosn $2) (L.tokStr $1) $3 }  
	 | print '(' Args ')' { S.Print (L.tokPosn $1) $3 }

Expr : int                { S.Int (L.tokPosn $1) (read $ L.tokStr $1) }
	 | ident              { S.Ident (L.tokPosn $1) (L.tokStr $1) }
	 | Expr '+' Expr      { S.Infix (L.tokPosn $2) S.Plus $1 $3 }
	 | Expr '-' Expr      { S.Infix (L.tokPosn $2) S.Minus $1 $3 }
	 | Expr '*' Expr      { S.Infix (L.tokPosn $2) S.Times $1 $3 }
	 | Expr '/' Expr      { S.Infix (L.tokPosn $2) S.Divide $1 $3 }
	 | Expr '%' Expr      { S.Infix (L.tokPosn $2) S.Mod $1 $3 }

Args : Expr               { [$1] }
	 | Expr ',' Args      { $1 : $3 }


{
data ParseResult a
	= ParseOk a 
	| ParseFail String
	deriving (Show)


type P a = Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
thenP m k = \l -> case m l of
	ParseFail s -> ParseFail s
	ParseOk a -> k a l


returnP :: a -> P a
returnP a = \l -> ParseOk a

happyError :: [L.Token] -> a
happyError x = error $ show x
}
