{
module Parser where
import qualified Lexer as L
import qualified AST as S
}

%name      parseTokens 
%tokentype { L.Token }
%error     { parseError }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  '<' '>'
%nonassoc  '<=' '>='
%nonassoc  '(' ')'


%token
    '='        { L.ReservedOp _ "=" }
    '+'        { L.ReservedOp _ "+" }
    '-'        { L.ReservedOp _ "-" }
    '*'        { L.ReservedOp _ "*" }
    '/'        { L.ReservedOp _ "/" }
    '%'        { L.ReservedOp _ "%" }
    '<'        { L.ReservedOp _ "<" }
    '>'        { L.ReservedOp _ ">" }
    ':='       { L.ReservedOp _ ":=" }
    '<='       { L.ReservedOp _ "<=" }
    '>='       { L.ReservedOp _ ">=" }
    '=='       { L.ReservedOp _ "==" }
    '&&'       { L.ReservedOp _ "&&" }
    '||'       { L.ReservedOp _ "||" }

    fn         { L.Reserved _ "fn" }
    if         { L.Reserved _ "if" }
    else       { L.Reserved _ "else" }
    for        { L.Reserved _ "for" }
    return     { L.Reserved _ "return" }
    print      { L.Reserved _ "print" }

    int        { L.Int _ _ }
    ident      { L.Ident _ _ }

    '('        { L.Sym _ '(' }
    ')'        { L.Sym _ ')' }
    '{'        { L.Sym _ '{' }
    '}'        { L.Sym _ '}' }
    ','        { L.Sym _ ',' }
    ';'        { L.Sym _ ';' }

%%

Prog : Stmt ';'           { [$1] }
	 | Stmt ';' Prog      { $1 : $3 }

Stmt : ident ':=' Expr    { let L.Ident _ s = $1 in S.Assign (L.tokPosn $2) s $3 }  
	 | print '(' Args ')' { S.Print (L.tokPosn $1) $3 }

Expr : int                { let L.Int pos n = $1 in S.Int pos n }
	 | ident              { let L.Ident p s = $1 in S.Ident p s }

Args : Expr               { [$1] }
	 | Expr ',' Args      { $1 : $3 }


{
parseError :: [L.Token] -> a
parseError (x:_) =
    error $ "parser error: " ++ show x
}
