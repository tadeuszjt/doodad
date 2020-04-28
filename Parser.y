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
    map        { L.Token _ L.Reserved "map" }
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
	'['        { L.Token _ L.Sym "[" }
	']'        { L.Token _ L.Sym "]" }
    ','        { L.Token _ L.Sym "," }
    ';'        { L.Token _ L.Sym ";" }
	 
	string     { L.Token _ L.String _ }

	%%

Prog : {- empty -}                  { [] }
     | Stmt                         { [$1] }
	 | Stmt Prog                    { $1 : $2 }

Stmt : StmtS ';'                    { $1 }
	 | StmtB                        { $1 }

StmtS : ident ':=' Expr             { S.Assign (tokPosn $2) (L.tokStr $1) $3 }  
	  | ident '=' Expr              { S.Set (tokPosn $2) (L.tokStr $1) $3 }
	  | print '(' Args ')'          { S.Print (tokPosn $1) $3 }
	  | ident '(' Args ')'          { S.CallStmt (tokPosn $1) (L.tokStr $1) $3 }
	  | return                      { S.Return (tokPosn $1) Nothing }
	  | return Expr                 { S.Return (tokPosn $1) (Just $2) }
	  | map ident Expr              { S.Map (tokPosn $1) (L.tokStr $2) $3 }

StmtB : Block                                     { $1 }
      | fn ident '(' Params ')' '{' Prog '}'      { S.Func (tokPosn $1) (L.tokStr $2) $4 Nothing $7 }
      | fn ident '(' Params ')' Type '{' Prog '}' { S.Func (tokPosn $1) (L.tokStr $2) $4 (Just $6) $8 }
	  | If                                        { $1 }


If : if Expr Block                  { S.If (tokPosn $1) $2 $3 Nothing }
   | if Expr Block Else             { S.If (tokPosn $1) $2 $3 (Just $4) }


Else : else Block                   { $2 }
     | else If                      { $2 }


Block : '{' Prog '}'                { S.Block (tokPosn $1) $2 }


Expr : int                          { S.Int (tokPosn $1) (read $ L.tokStr $1) }
	 | true                         { S.Bool (tokPosn $1) True }
	 | false                        { S.Bool (tokPosn $1) False }
	 | ident                        { S.Ident (tokPosn $1) (L.tokStr $1) }
	 | string                       { S.String (tokPosn $1) (L.tokStr $1) }
	 | ident '(' Args ')'           { S.Call (tokPosn $1) (L.tokStr $1) $3 }
	 | '-' Expr                     { S.Prefix (tokPosn $1) S.Minus $2 }
	 | '[' Args ']'                 { S.Array (tokPosn $1) $2 }
	 | Expr '+' Expr                { S.Infix (tokPosn $2) S.Plus $1 $3 }
	 | Expr '-' Expr                { S.Infix (tokPosn $2) S.Minus $1 $3 }
	 | Expr '*' Expr                { S.Infix (tokPosn $2) S.Times $1 $3 }
	 | Expr '/' Expr                { S.Infix (tokPosn $2) S.Divide $1 $3 }
	 | Expr '%' Expr                { S.Infix (tokPosn $2) S.Mod $1 $3 }
	 | Expr '<' Expr                { S.Infix (tokPosn $2) S.LT $1 $3 }
	 | Expr '>' Expr                { S.Infix (tokPosn $2) S.GT $1 $3 }
	 | Expr '<=' Expr               { S.Infix (tokPosn $2) S.LTEq $1 $3 }
	 | Expr '>=' Expr               { S.Infix (tokPosn $2) S.GTEq $1 $3 }
	 | Expr '==' Expr               { S.Infix (tokPosn $2) S.EqEq $1 $3 }
	 | Expr '&&' Expr               { S.Infix (tokPosn $2) S.AndAnd $1 $3 }
	 | Expr '||' Expr               { S.Infix (tokPosn $2) S.OrOr $1 $3 }

Args : {- empty -}                  { [] }
	 | Args_                        { $1 }

Args_ : Expr                        { [$1] }
	  | Expr ',' Args_              { $1 : $3 }

Params : {- empty -}                { [] }
       | Params_                    { $1 }

Params_ : Param                     { [$1] }
		| Param ',' Params_         { $1 : $3 }

Param : ident Type                  { S.Param (tokPosn $1) (L.tokStr $1) $2 }

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
