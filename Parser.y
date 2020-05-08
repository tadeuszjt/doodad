{
module Parser where
import qualified Lexer as L
import qualified AST as S
import qualified Cmp as C
}

%name      parseTokens 
%tokentype { L.Token }
%monad { P } { thenP } { returnP }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%left      '.'
%nonassoc  '<' '>'
%nonassoc  '<=' '>='
%nonassoc  '(' ')'
%nonassoc  '[' ']'


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
    extern     { L.Token _ L.Reserved "extern" }
    type       { L.Token _ L.Reserved "type" }
    if         { L.Token _ L.Reserved "if" }
    else       { L.Token _ L.Reserved "else" }
    for        { L.Token _ L.Reserved "for" }
    return     { L.Token _ L.Reserved "return" }
    switch     { L.Token _ L.Reserved "switch" }
	true       { L.Token _ L.Reserved "true" }
	false      { L.Token _ L.Reserved "false" }

    print      { L.Token _ L.Reserved "print" }
	len        { L.Token _ L.Reserved "len" }

	i64        { L.Token _ L.Reserved "i64" }
	i32        { L.Token _ L.Reserved "i32" }
    f64        { L.Token _ L.Reserved "f64" }
    f32        { L.Token _ L.Reserved "f32" }
	bool       { L.Token _ L.Reserved "bool" }
	char       { L.Token _ L.Reserved "char" }
	string     { L.Token _ L.Reserved "string" }

    int        { L.Token _ L.Int _ }
	floatlit   { L.Token _ L.Float _ }
	charlit    { L.Token _ L.Char _ }
	strlit     { L.Token _ L.String _ }
    ident      { L.Token _ L.Ident _ }

    '('        { L.Token _ L.Sym "(" }
    ')'        { L.Token _ L.Sym ")" }
    '{'        { L.Token _ L.Sym "{" }
    '}'        { L.Token _ L.Sym "}" }
	'['        { L.Token _ L.Sym "[" }
	']'        { L.Token _ L.Sym "]" }
    ','        { L.Token _ L.Sym "," }
    '.'        { L.Token _ L.Sym "." }
    ';'        { L.Token _ L.Sym ";" }
    ':'        { L.Token _ L.Sym ":" }
    '_'        { L.Token _ L.Sym "_" }
	 

	%%
Prog : {- empty -}                  { [] }
     | Stmt                         { [$1] }
	 | Stmt Prog                    { $1 : $2 }


Stmt  : StmtS ';'                                 { $1 }
	  | StmtB                                     { $1 }
StmtS : Pattern ':=' Expr                         { S.Assign (tokPosn $2) $1 $3 }  
	  | ident '=' Expr                            { S.Set (tokPosn $2) (L.tokStr $1) $3 }
	  | print '(' Args ')'                        { S.Print (tokPosn $1) $3 }
	  | ident '(' Args ')'                        { S.CallStmt (tokPosn $1) (L.tokStr $1) $3 }
	  | return                                    { S.Return (tokPosn $1) Nothing }
	  | return Expr                               { S.Return (tokPosn $1) (Just $2) }
	  | extern ident '(' Params ')' Type          { S.Extern (tokPosn $2) (L.tokStr $2) $4 (Just $6) }
	  | extern ident '(' Params ')'               { S.Extern (tokPosn $2) (L.tokStr $2) $4 Nothing }
      | type ident '=' Type                       { S.Typedef (tokPosn $2) (L.tokStr $2) $4 }
StmtB : Block                                     { $1 }
      | fn ident '(' Params ')' '{' Prog '}'      { S.Func (tokPosn $1) (L.tokStr $2) $4 Nothing $7 }
      | fn ident '(' Params ')' Type '{' Prog '}' { S.Func (tokPosn $1) (L.tokStr $2) $4 (Just $6) $8 }
	  | switch Expr '{' Cases '}'                 { S.Switch (tokPosn $1) $2 $4 }
	  | If                                        { $1 }


Type         : ConcreteType         { $1 }
             | ident                { S.TIdent (L.tokStr $1) }
ConcreteType : bool                 { S.TBool }
	         | i32                  { S.TI32 }
             | i64                  { S.TI64 }
             | f32                  { S.TF32 }
             | f64                  { S.TF64 }
	         | char                 { S.TChar }
	         | string               { S.TString }
	         | '[' int Type ']'     { S.TArray (read $ L.tokStr $2) $3 }
	         | '(' Types ')'        { S.TTuple $2 }
Types        : {- empty -}          { [] }
	         | Types_               { $1 }
Types_       : Type                 { [$1] }
	         | Type ',' Types_      { $1 : $3 }


Expr : int                          { S.Int (tokPosn $1) (read $ L.tokStr $1) }
	 | floatlit                     { S.Float (tokPosn $1) (read $ L.tokStr $1) }
	 | charlit                      { S.Char (tokPosn $1) (read $ L.tokStr $1) }
	 | strlit                       { S.String (tokPosn $1) (L.tokStr $1) }
	 | true                         { S.Bool (tokPosn $1) True }
	 | false                        { S.Bool (tokPosn $1) False }
	 | ident                        { S.Ident (tokPosn $1) (L.tokStr $1) }
	 | '[' Args ']'                 { S.Array (tokPosn $1) $2 }
	 | '(' Args ')'                 { S.Tuple (tokPosn $1) $2 }
	 | ident '(' Args ')'           { S.Call (tokPosn $1) (L.tokStr $1) $3 }
	 | len '(' Expr ')'             { S.Len (tokPosn $1) $3 }
	 | Expr '.' int                 { S.TupleIndex (tokPosn $2) $1 (read $ L.tokStr $3) }
     | Expr '[' Expr ']'            { S.ArrayIndex (tokPosn $2) $1 $3 }
	 | '-' Expr                     { S.Prefix (tokPosn $1) S.Minus $2 }
	 | '+' Expr                     { S.Prefix (tokPosn $1) S.Plus $2 }
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


Pattern   : '_'                     { S.PatIgnore (tokPosn $1) }
		  | ident                   { S.PatIdent (tokPosn $1) (L.tokStr $1) }
		  | '(' Patterns ')'        { S.PatTuple (tokPosn $1) $2 }
		  | '[' Patterns ']'        { S.PatArray (tokPosn $1) $2 }
Patterns  : {- empty -}             { [] }
          | Patterns_               { $1 }
Patterns_ : Pattern                 { [$1] }
		  | Pattern ',' Patterns_   { $1 : $3 }


Switch : switch Expr '{' Cases '}'  { S.Switch (tokPosn $1) $2 $4 }
Cases  : {- empty -}                { [] }
	   | Case Cases                 { $1 : $2 }
Case   : Expr ':' Stmt              { (Just $1, $3) }
	   | '_'  ':' Stmt              { (Nothing, $3) }


If   : if Expr Block                { S.If (tokPosn $1) $2 $3 Nothing }
     | if Expr Block Else           { S.If (tokPosn $1) $2 $3 (Just $4) }
Else : else Block                   { $2 }
     | else If                      { $2 }


Block : '{' Prog '}'                { S.Block (tokPosn $1) $2 }


Args  : {- empty -}                 { [] }
	  | Args_                       { $1 }
Args_ : Expr                        { [$1] }
	  | Expr ',' Args_              { $1 : $3 }


Param   : ident Type                { S.Param (tokPosn $1) (L.tokStr $1) $2 }
Params  : {- empty -}               { [] }
        | Params_                   { $1 }
Params_ : Param                     { [$1] }
		| Param ',' Params_         { $1 : $3 }

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
