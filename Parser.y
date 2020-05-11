{
module Parser where
import qualified Lexer as L
import qualified AST as S
import qualified CmpMonad as C
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
%nonassoc  ','
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
    data       { L.Token _ L.Reserved "data" }
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


Prog  : {- empty -}                         { [] }
      | Stmt Prog                           { $1 : $2 }


Stmt  : StmtS ';'                           { $1 }
      | StmtB                               { $1 }
StmtS : pattern ':=' expr                   { S.Assign (tokPosn $2) $1 $3 }  
      | Index '=' expr                      { S.Set (tokPosn $2) $1 $3 }
      | type ident '=' Type                 { S.Typedef (tokPosn $2) (L.tokStr $2) $4 }
      | data ident '=' Datas                { S.Datadef (tokPosn $2) (L.tokStr $2) $4 }
      | extern ident '(' Params ')' Type    { S.Extern (tokPosn $2) (L.tokStr $2) $4 (Just $6) }
      | extern ident '(' Params ')'         { S.Extern (tokPosn $2) (L.tokStr $2) $4 Nothing }
      | print '(' args ')'                  { S.Print (tokPosn $1) $3 }
      | ident '(' args ')'                  { S.CallStmt (tokPosn $1) (L.tokStr $1) $3 }
      | return                              { S.Return (tokPosn $1) Nothing }
      | return expr                         { S.Return (tokPosn $1) (Just $2) }
StmtB : Block                               { $1 }
      | If                                  { $1 }
      | fn ident '(' Params ')' Block_      { S.Func (tokPosn $1) (L.tokStr $2) $4 Nothing $6 }
      | fn ident '(' Params ')' Type Block_ { S.Func (tokPosn $1) (L.tokStr $2) $4 (Just $6) $7 }
      | switch expr '{' Cases '}'           { S.Switch (tokPosn $1) $2 $4 }


expr : int                          { S.Int (tokPosn $1) (read $ L.tokStr $1) }
     | floatlit                     { S.Float (tokPosn $1) (read $ L.tokStr $1) }
     | charlit                      { S.Char (tokPosn $1) (read $ L.tokStr $1) }
     | strlit                       { S.String (tokPosn $1) (L.tokStr $1) }
     | true                         { S.Bool (tokPosn $1) True }
     | false                        { S.Bool (tokPosn $1) False }
     | ident                        { S.Ident (tokPosn $1) (L.tokStr $1) }
     | '[' args ']'                 { S.Array (tokPosn $1) $2 }
     | '(' args ')'                 { S.Tuple (tokPosn $1) $2 }
     | ident '(' args ')'           { S.Call (tokPosn $1) (L.tokStr $1) $3 }
     | len '(' expr ')'             { S.Len (tokPosn $1) $3 }
     | expr '.' int                 { S.TupleIndex (tokPosn $2) $1 (read $ L.tokStr $3) }
     | expr '[' expr ']'            { S.ArrayIndex (tokPosn $2) $1 $3 }
     | '-' expr                     { S.Prefix (tokPosn $1) S.Minus $2 }
     | '+' expr                     { S.Prefix (tokPosn $1) S.Plus $2 }
     | expr '+' expr                { S.Infix (tokPosn $2) S.Plus $1 $3 }
     | expr '-' expr                { S.Infix (tokPosn $2) S.Minus $1 $3 }
     | expr '*' expr                { S.Infix (tokPosn $2) S.Times $1 $3 }
     | expr '/' expr                { S.Infix (tokPosn $2) S.Divide $1 $3 }
     | expr '%' expr                { S.Infix (tokPosn $2) S.Mod $1 $3 }
     | expr '<' expr                { S.Infix (tokPosn $2) S.LT $1 $3 }
     | expr '>' expr                { S.Infix (tokPosn $2) S.GT $1 $3 }
     | expr '<=' expr               { S.Infix (tokPosn $2) S.LTEq $1 $3 }
     | expr '>=' expr               { S.Infix (tokPosn $2) S.GTEq $1 $3 }
     | expr '==' expr               { S.Infix (tokPosn $2) S.EqEq $1 $3 }
     | expr '&&' expr               { S.Infix (tokPosn $2) S.AndAnd $1 $3 }
     | expr '||' expr               { S.Infix (tokPosn $2) S.OrOr $1 $3 }


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


Data   : ident                      { S.DataIdent (tokPosn $1) (L.tokStr $1) }
Datas  : Data                       { [$1] }
       | Data ',' Datas             { $1 : $3 }


pattern   : '_'                     { S.PatIgnore (tokPosn $1) }
          | ident                   { S.PatIdent (tokPosn $1) (L.tokStr $1) }
          | '(' patterns ')'        { S.PatTuple (tokPosn $1) $2 }
          | '[' patterns ']'        { S.PatArray (tokPosn $1) $2 }
patterns  : pattern                 { [$1] }
          | pattern ',' patterns    { $1 : $3 }


Index  : ident                      { S.IndIdent (tokPosn $1) (L.tokStr $1) }
       | Index '[' expr ']'         { S.IndArray (tokPosn $2) $1 $3 }
       | Index '.' int              { S.IndTuple (tokPosn $2) $1 (read $ L.tokStr $3) }


Switch : switch expr '{' Cases '}'  { S.Switch (tokPosn $1) $2 $4 }
Cases  : {- empty -}                { [] }
       | Case Cases                 { $1 : $2 }
Case   : expr ':' Stmt              { (Just $1, $3) }
       | '_'  ':' Stmt              { (Nothing, $3) }


If   : if expr Block                { S.If (tokPosn $1) $2 $3 Nothing }
     | if expr Block Else           { S.If (tokPosn $1) $2 $3 (Just $4) }
Else : else Block                   { $2 }
     | else If                      { $2 }


Block  : '{' Prog '}'               { S.Block (tokPosn $1) $2 }
Block_ : '{' Prog '}'               { $2 }


args  : {- empty -}                 { [] }
      | args_                       { $1 }
args_ : expr                        { [$1] }
      | expr ',' args_              { $1 : $3 }


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
