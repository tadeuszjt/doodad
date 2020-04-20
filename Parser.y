{
module Parser where
import qualified Lexer as L
import qualified AST as S
import qualified CmpState as C
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
    '+'        { L.Token _ L.ReservedOp "+" }
    '-'        { L.Token _ L.ReservedOp "-" }
    '*'        { L.Token _ L.ReservedOp "*" }
    '/'        { L.Token _ L.ReservedOp "/" }
    '%'        { L.Token _ L.ReservedOp "%" }
    '<'        { L.Token _ L.ReservedOp "<" }
    '>'        { L.Token _ L.ReservedOp ">" }
    '='        { L.Token _ L.ReservedOp "=" }
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
	true       { L.Token _ L.Reserved "true" }
	false      { L.Token _ L.Reserved "false" }

	i64        { L.Token _ L.Reserved "i64" }
	bool       { L.Token _ L.Reserved "bool" }

    int        { L.Token _ L.Int _ }
    ident      { L.Token _ L.Ident _ }

    '('        { L.Token _ L.Sym "(" }
    ')'        { L.Token _ L.Sym ")" }
    '{'        { L.Token _ L.Sym "{" }
    '}'        { L.Token _ L.Sym "}" }
    ','        { L.Token _ L.Sym "," }
    ';'        { L.Token _ L.Sym ";" }

%%

Prog : Stmt                         { [$1] }
	 | Stmt Prog                    { $1 : $2 }

Stmt : StmtS ';'                    { $1 }
	 | StmtB                        { $1 }

StmtS : ident ':=' Expr             { S.Assign (tokPosn $2) (L.tokStr $1) $3 }  
	  | ident '=' Expr              { S.Set (tokPosn $2) (L.tokStr $1) $3 }
	  | print '(' Args ')'          { S.Print (tokPosn $1) $3 }
	  | ident '(' Args ')'          { S.CallStmt (tokPosn $1) (L.tokStr $1) $3 }
	  | return                      { S.Return (tokPosn $1) Nothing }
	  | return Expr                 { S.Return (tokPosn $1) (Just $2) }

StmtB : Block                       { $1 }
      | fn ident '(' ')' Block      { S.Func (tokPosn $1) (L.tokStr $2) Nothing $5 }
      | fn ident '(' ')' Type Block { S.Func (tokPosn $1) (L.tokStr $2) (Just $5) $6 }
	  | if Expr Block               { S.If (tokPosn $1) $2 $3 }

Block : '{' Prog '}'                { S.Block (tokPosn $1) $2 }


Expr : int                          { S.Int (tokPosn $1) (read $ L.tokStr $1) }
	 | true                         { S.Bool (tokPosn $1) True }
	 | false                        { S.Bool (tokPosn $1) False }
	 | ident                        { S.Ident (tokPosn $1) (L.tokStr $1) }
	 | ident '(' Args ')'           { S.Call (tokPosn $1) (L.tokStr $1) $3 }
	 | Expr '+' Expr                { S.Infix (tokPosn $2) S.Plus $1 $3 }
	 | Expr '-' Expr                { S.Infix (tokPosn $2) S.Minus $1 $3 }
	 | Expr '*' Expr                { S.Infix (tokPosn $2) S.Times $1 $3 }
	 | Expr '/' Expr                { S.Infix (tokPosn $2) S.Divide $1 $3 }
	 | Expr '%' Expr                { S.Infix (tokPosn $2) S.Mod $1 $3 }

Args : {- empty -}                  { [] }
	 | Args_                        { $1 }

Args_ : Expr                        { [$1] }
	  | Expr ',' Args_              { $1 : $3 }

Type : i64                          { S.I64 }
	 | bool                         { S.TBool }

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

tokPosn :: L.Token -> C.TextPos
tokPosn tok = C.TextPos
	{ C.textPos  = pos
	, C.textLine = line
	, C.textCol  = col
	}
	where
		L.AlexPn pos line col = L.tokPosn tok
}
