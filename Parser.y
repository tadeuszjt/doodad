{
module Parser where
import qualified Type     as T
import qualified Lexer    as L
import qualified AST      as S
import qualified CmpMonad as C
import qualified Error    as C
import qualified Data.Set as Set
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
    if         { L.Token _ L.Reserved "if" }
    else       { L.Token _ L.Reserved "else" }
    let        { L.Token _ L.Reserved "let" }
    while      { L.Token _ L.Reserved "while" }
    return     { L.Token _ L.Reserved "return" }
    switch     { L.Token _ L.Reserved "switch" }
    true       { L.Token _ L.Reserved "true" }
    false      { L.Token _ L.Reserved "false" }
    module     { L.Token _ L.Reserved "module" }
    imports    { L.Token _ L.Reserved "imports" }

    print      { L.Token _ L.Reserved "print" }
    len        { L.Token _ L.Reserved "len" }
    append     { L.Token _ L.Reserved "append" }

    i64        { L.Token _ L.Reserved "i64" }
    i32        { L.Token _ L.Reserved "i32" }
    f64        { L.Token _ L.Reserved "f64" }
    f32        { L.Token _ L.Reserved "f32" }
    bool       { L.Token _ L.Reserved "bool" }
    char       { L.Token _ L.Reserved "char" }
    string     { L.Token _ L.Reserved "string" }

    intlit     { L.Token _ L.Int _ }
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
    '|'        { L.Token _ L.Sym "|" }
    ','        { L.Token _ L.Sym "," }
    '.'        { L.Token _ L.Sym "." }
    ';'        { L.Token _ L.Sym ";" }
    ':'        { L.Token _ L.Sym ":" }
    '_'        { L.Token _ L.Sym "_" }
     

    %%


Prog  : Prog_                                { S.AST { S.astModuleName = Nothing, S.astStmts = $1, S.astImports = Set.empty} }
      | module ident Imports Prog_           { S.AST { S.astModuleName = Just (L.tokStr $2), S.astStmts = $4, S.astImports = Set.fromList $3 } }
Prog_ : {- empty -}                          { [] }
      | stmt Prog_                           { $1 : $2 }

Imports  : {- empty -}                       { [] }
         | Imports_                          { $1 }
Imports_ : imports strlit                    { [(L.tokStr $2)] }
         | imports strlit Imports_           { (L.tokStr $2) : $3 }


stmt  : stmtS ';'                            { $1 }
      | stmtB                                { $1 }
stmtS : let pattern '=' expr                 { S.Assign (tokPosn $1) $2 $4 }  
      | index '=' expr                       { S.Set (tokPosn $2) $1 $3 }
      | type ident '=' type_                 { S.Typedef (tokPosn $2) (L.tokStr $2) $4 }
      | extern ident '(' params ')' type_    { S.Extern (tokPosn $2) (L.tokStr $2) $4 (Just $6) }
      | extern ident '(' params ')'          { S.Extern (tokPosn $2) (L.tokStr $2) $4 Nothing }
      | ident '(' exprs ')'                  { S.CallStmt (tokPosn $1) (L.tokStr $1) $3 }
      | print '(' exprs ')'                  { S.Print (tokPosn $1) $3 }
      | return                               { S.Return (tokPosn $1) Nothing }
      | return expr                          { S.Return (tokPosn $1) (Just $2) }
stmtB : block                                { $1 }
      | If                                   { $1 }
      | fn ident '(' params ')' block_       { S.Func (tokPosn $1) (L.tokStr $2) $4 Nothing $6 }
      | fn ident '(' params ')' type_ block_ { S.Func (tokPosn $1) (L.tokStr $2) $4 (Just $6) $7 }
      | switch expr '{' cases '}'            { S.Switch (tokPosn $1) $2 $4 }
      | while expr block_                    { S.While (tokPosn $1) $2 $3 }


expr   : lit                          { S.Cons $1 }
       | infix                        { $1 }
       | ident                        { S.Ident (tokPosn $1) (L.tokStr $1) }
       | table                        { $1 }
       | '[' exprs ']'                { S.Array (tokPosn $1) $2 }
       | '(' exprs ')'                { S.Tuple (tokPosn $1) $2 }
       | ident '(' exprs ')'          { S.Call (tokPosn $1) (L.tokStr $1) $3 }
       | type__ '(' exprs ')'         { S.Conv (tokPosn $2) $1 $3 }
       | len '(' expr ')'             { S.Len (tokPosn $1) $3 }
       | append '(' expr ',' expr ')' { S.Append (tokPosn $1) $3 $5 }
       | expr '.' intlit              { S.TupleIndex (tokPosn $2) $1 (read $ L.tokStr $3) }
       | expr '.' ident               { S.Member (tokPosn $2) $1 (L.tokStr $3) }
       | expr '[' expr ']'            { S.Subscript (tokPosn $2) $1 $3 }
       | '-' expr                     { S.Prefix (tokPosn $1) S.Minus $2 }
       | '+' expr                     { S.Prefix (tokPosn $1) S.Plus $2 }
exprs  : {- empty -}                  { [] }
       | exprs_                       { $1 }
exprs_ : expr                         { [$1] }
       | expr ',' exprs_              { $1 : $3 }


lit : intlit                         { S.Int (tokPosn $1) (read $ L.tokStr $1) }
    | floatlit                       { S.Float (tokPosn $1) (read $ L.tokStr $1) }
    | charlit                        { S.Char (tokPosn $1) (read $ L.tokStr $1) }
    | strlit                         { S.String (tokPosn $1) (L.tokStr $1) }
    | true                           { S.Bool (tokPosn $1) True }
    | false                          { S.Bool (tokPosn $1) False }


infix : expr '+' expr                { S.Infix (tokPosn $2) S.Plus $1 $3 }
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


type_         : type__               { $1 }
              | ident                { T.Typedef (L.tokStr $1) }
type__        : concreteType         { $1 }
              | aggregateType        { $1 }
types         : {- empty -}          { [] }
              | types_               { $1 }
types_        : type_                { [$1] }
              | type_ ',' types_     { $1 : $3 }
concreteType  : bool                 { T.Bool }
              | i32                  { T.I32 }
              | i64                  { T.I64 }
              | f32                  { T.F32 }
              | f64                  { T.F64 }
              | char                 { T.Char }
              | string               { T.String }
anno          : ident type_          { T.Annotated (L.tokStr $1) $2 }
annos_        : anno                 { [$1] }
              | anno ',' annos_      { $1 : $3 }
aggregateType : '[' intlit type_ ']' { T.Array (read $ L.tokStr $2) $3 }
              | '(' types ')'        { T.Tuple $2 }
              | '(' annos_ ')'       { T.Tuple $2 }
              | '{' rowTypes '}'     { T.Table $2 }
rowTypes      : {- empty -}          { [] }
              | rowTypes_            { $1 }
rowTypes_     : type_                { [$1] }
              | type_ ';' rowTypes_  { $1 : $3 }


table     : '{' tableRows '}'       { S.Table (tokPosn $1) $2 } 
tableRows : exprs                   { [$1] }
          | exprs ';' tableRows     { $1 : $3 }


pattern   : pattern_                { $1 }
pattern_  : '_'                     { S.PatIgnore (tokPosn $1) }
          | lit                     { S.PatLiteral $1 }
          | ident                   { S.PatIdent (tokPosn $1) (L.tokStr $1) }
          | ident pattern           { S.PatTyped (tokPosn $1) (L.tokStr $1) $2 }
          | '(' patterns ')'        { S.PatTuple (tokPosn $1) $2 }
          | '[' patterns ']'        { S.PatArray (tokPosn $1) $2 }
patterns  : {- empty -}             { [] }
          | patterns_               { $1 }
patterns_ : pattern_                { [$1] }
          | pattern_ ',' patterns_  { $1 : $3 }


param   : ident type_               { S.Param (tokPosn $1) (L.tokStr $1) $2 }
params  : {- empty -}               { [] }
        | params_                   { $1 }
params_ : param                     { [$1] }
        | param ',' params_         { $1 : $3 }


index  : ident                      { S.IndIdent (tokPosn $1) (L.tokStr $1) }
       | index '[' expr ']'         { S.IndArray (tokPosn $2) $1 $3 }
       | index '.' intlit           { S.IndTuple (tokPosn $2) $1 (read $ L.tokStr $3) }


Switch : switch expr '{' cases '}'  { S.Switch (tokPosn $1) $2 $4 }
cases  : {- empty -}                { [] }
       | case cases                 { $1 : $2 }
case   : pattern ':' stmt           { ($1, $3) }


If    : if expr block                { S.If (tokPosn $1) $2 $3 Nothing }
      | if expr block else_           { S.If (tokPosn $1) $2 $3 (Just $4) }
else_ : else block                   { $2 }
      | else If                      { $2 }


block  : '{' Prog_ '}'               { S.Block (tokPosn $1) $2 }
block_ : '{' Prog_ '}'               { $2 }


{
data ParseResult a
    = ParseOk a 
    | ParseFail C.TextPos
    deriving (Show)


type P a = Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
thenP m k = \l -> case m l of
    ParseFail s -> ParseFail s
    ParseOk a -> k a l


returnP :: a -> P a
returnP a = \l -> ParseOk a


happyError :: [L.Token] -> P a
happyError []    = return $ ParseFail (C.TextPos 0 0 0)
happyError (x:_) = return $ ParseFail (tokPosn x)


tokPosn :: L.Token -> C.TextPos
tokPosn tok = C.TextPos
    { C.textPos  = pos
    , C.textLine = line
    , C.textCol  = col
    }
    where
        L.AlexPn pos line col = L.tokPosn tok
}
