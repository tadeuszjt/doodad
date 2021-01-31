{
{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Lexer
import Error
import Control.Monad.Except hiding (void, fail)
import qualified Type as T
import qualified AST as S
import qualified Data.Set as Set
}

%name      parseTokens 
%tokentype { Token }
%monad { P } { thenP } { returnP }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%left      '.'
%nonassoc  '&'
%nonassoc  '!'
%nonassoc  ','
%nonassoc  '<' '>'
%nonassoc  '<=' '>='
%nonassoc  '(' ')'
%nonassoc  '[' ']'
%nonassoc  '{' '}'
%nonassoc  '|'


%token
	'I'        { Token _ Indent _ }
	'D'        { Token _ Dedent _ }
	'N'        { Token _ NewLine _ }

    '+'        { Token _ ReservedOp "+" }
    '-'        { Token _ ReservedOp "-" }
    '*'        { Token _ ReservedOp "*" }
    '/'        { Token _ ReservedOp "/" }
    '%'        { Token _ ReservedOp "%" }
    '<'        { Token _ ReservedOp "<" }
    '>'        { Token _ ReservedOp ">" }
    '='        { Token _ ReservedOp "=" }
    '&'        { Token _ ReservedOp "&" }
    '!'        { Token _ ReservedOp "!" }
    '!='       { Token _ ReservedOp "!=" }
    '<='       { Token _ ReservedOp "<=" }
    '>='       { Token _ ReservedOp ">=" }
    '=='       { Token _ ReservedOp "==" }
    '&&'       { Token _ ReservedOp "&&" }
    '||'       { Token _ ReservedOp "||" }
    '..'       { Token _ ReservedOp ".." }

    fn         { Token _ Reserved "fn" }
    extern     { Token _ Reserved "extern" }
    type       { Token _ Reserved "type" }
    if         { Token _ Reserved "if" }
    else       { Token _ Reserved "else" }
    let        { Token _ Reserved "let" }
    while      { Token _ Reserved "while" }
    return     { Token _ Reserved "return" }
    switch     { Token _ Reserved "switch" }
    true       { Token _ Reserved "true" }
    false      { Token _ Reserved "false" }
    module     { Token _ Reserved "module" }
    imports    { Token _ Reserved "imports" }

    print      { Token _ Reserved "print" }
    len        { Token _ Reserved "len" }
    append     { Token _ Reserved "append" }
    null       { Token _ Reserved "null" }

    i16        { Token _ Reserved "i16" }
    i32        { Token _ Reserved "i32" }
    i64        { Token _ Reserved "i64" }
    f32        { Token _ Reserved "f32" }
    f64        { Token _ Reserved "f64" }
    bool       { Token _ Reserved "bool" }
    char       { Token _ Reserved "char" }
    string     { Token _ Reserved "string" }

    intlit     { Token _ Int _ }
    floatlit   { Token _ Float _ }
    charlit    { Token _ Char _ }
    strlit     { Token _ String _ }
    ident      { Token _ Ident _ }

    '('        { Token _ Sym "(" }
    ')'        { Token _ Sym ")" }
    '{'        { Token _ Sym "{" }
    '}'        { Token _ Sym "}" }
    '['        { Token _ Sym "[" }
    ']'        { Token _ Sym "]" }
    '|'        { Token _ Sym "|" }
    ','        { Token _ Sym "," }
    '.'        { Token _ Sym "." }
    ';'        { Token _ Sym ";" }
    ':'        { Token _ Sym ":" }
    '_'        { Token _ Sym "_" }
     

    %%


Prog  : Prog_                                { S.AST Nothing [] $1 }
      | module ident 'N' Imports Prog_       { S.AST (Just (tokStr $2)) $4 $5 }
Prog_ : stmtS                                { [$1] }
      | stmtB                                { [$1] }
      | stmtS 'N' Prog_                      { $1 : $3 }
      | stmtB  Prog_                         { $1 : $2 }


Imports : {- empty -}                      { [] }
        | imports importPath 'N' Imports   { $2 : $4 }


importPath : ident                           { [tokStr $1] }
           | '..'                            { [".."] }
           | importPath '/' importPath       { $1 ++ $3 }


stmtS : let pattern '=' expr                 { S.Assign (tokPos $1) $2 $4 }  
      | index '=' expr                       { S.Set (tokPos $2) $1 $3 }
      | type ident type_                     { S.Typedef (tokPos $2) (tokStr $2) $3 }
      | extern ident '(' params ')' type_    { S.Extern (tokPos $2) (tokStr $2) $4 $6 }
      | extern ident '(' params ')'          { S.Extern (tokPos $2) (tokStr $2) $4 T.Void }
      | ident '(' exprs ')'                  { S.CallStmt (tokPos $1) (tokStr $1) $3 }
      | print '(' exprs ')'                  { S.Print (tokPos $1) $3 }
      | return                               { S.Return (tokPos $1) Nothing }
      | return expr                          { S.Return (tokPos $1) (Just $2) }
stmtB : block                                { $1 }
      | If                                   { $1 }
      | fn ident '(' params ')' block_       { S.Func (tokPos $1) (tokStr $2) $4 T.Void $6 }
      | fn ident '(' params ')' type_ block_ { S.Func (tokPos $1) (tokStr $2) $4 $6 $7 }
      | switch expr 'I' cases 'D'            { S.Switch (tokPos $1) $2 $4 }
      | while expr block_                    { S.While (tokPos $1) $2 $3 }

block  : 'I' Prog_ 'D'               { S.Block (tokPos $1) $2 }
block_ : 'I' Prog_ 'D'               { $2 }


lit : intlit                         { S.Int (tokPos $1) (read $ tokStr $1) }
    | floatlit                       { S.Float (tokPos $1) (read $ tokStr $1) }
    | charlit                        { S.Char (tokPos $1) (read $ tokStr $1) }
    | strlit                         { S.String (tokPos $1) (tokStr $1) }
    | true                           { S.Bool (tokPos $1) True }
    | false                          { S.Bool (tokPos $1) False }
    | null                           { S.Null (tokPos $1) }

expr   : lit                          { $1 }
       | infix                        { $1 }
       | prefix                       { $1 }
       | ident                        { S.Ident (tokPos $1) (tokStr $1) }
       | '[' tableRows ']'            { S.Table (tokPos $1) $2 }
       | '[' '|' exprs ']'            { S.Array (tokPos $1) $3 }
       | '(' exprs ')'                { S.Tuple (tokPos $1) $2 }
       | '&' expr                     { S.Address (tokPos $1) $2 }
       | ident '(' exprs ')'          { S.Call (tokPos $1) (tokStr $1) $3 }
       | typeNoIdent '(' exprs ')'    { S.Conv (tokPos $2) $1 $3 }
       | len '(' expr ')'             { S.Len (tokPos $1) $3 }
       | append '(' expr ',' expr ')' { S.Append (tokPos $1) $3 $5 }
       | expr '.' intlit              { S.TupleIndex (tokPos $2) $1 (read $ tokStr $3) }
       | expr '.' ident               { S.Member (tokPos $2) $1 (tokStr $3) }
       | expr '[' expr ']'            { S.Subscript (tokPos $2) $1 $3 }
       | expr '[' expr '..' ']'       { S.Range (tokPos $2) $1 (Just $3) Nothing }
       | expr '[' '..' expr ']'       { S.Range (tokPos $2) $1 Nothing (Just $4) }
exprs  : {- empty -}                  { [] }
       | exprs_                       { $1 }
exprs_ : expr                         { [$1] }
       | expr ',' exprs_              { $1 : $3 }
tableRows : exprs                   { [$1] }
          | exprs ';' tableRows     { $1 : $3 }



prefix : '-' expr                    { S.Prefix (tokPos $1) S.Minus $2 }
       | '+' expr                    { S.Prefix (tokPos $1) S.Plus $2 }
       | '!' expr                    { S.Prefix (tokPos $1) S.Not $2 }

infix : expr '+' expr                { S.Infix (tokPos $2) S.Plus $1 $3 }
      | expr '-' expr                { S.Infix (tokPos $2) S.Minus $1 $3 }
      | expr '*' expr                { S.Infix (tokPos $2) S.Times $1 $3 }
      | expr '/' expr                { S.Infix (tokPos $2) S.Divide $1 $3 }
      | expr '%' expr                { S.Infix (tokPos $2) S.Mod $1 $3 }
      | expr '<' expr                { S.Infix (tokPos $2) S.LT $1 $3 }
      | expr '>' expr                { S.Infix (tokPos $2) S.GT $1 $3 }
      | expr '<=' expr               { S.Infix (tokPos $2) S.LTEq $1 $3 }
      | expr '>=' expr               { S.Infix (tokPos $2) S.GTEq $1 $3 }
      | expr '==' expr               { S.Infix (tokPos $2) S.EqEq $1 $3 }
      | expr '&&' expr               { S.Infix (tokPos $2) S.AndAnd $1 $3 }
      | expr '||' expr               { S.Infix (tokPos $2) S.OrOr $1 $3 }
      | expr '!=' expr               { S.Infix (tokPos $2) S.NotEq $1 $3 }

type_         : typeNoIdent          { $1 }
              | ident                { T.Typedef (tokStr $1) }
typeNoIdent   : bool                 { T.Bool }
              | i16                  { T.I16 }
              | i32                  { T.I32 }
              | i64                  { T.I64 }
              | f32                  { T.F32 }
              | f64                  { T.F64 }
              | char                 { T.Char }
              | string               { T.String }
              | '[' intlit ':' type_ ']'       { T.Array (read $ tokStr $2) $4 }
              | '(' tupTypes ')'               { T.Tuple $2 }
              | '[' rowTypes_ ']'              { T.Table $2 }
              | '{' ptrTypes '}'               { T.Pointer $2 }

types         : {- empty -}          { [] }
              | types_               { $1 }
types_        : type_                { [$1] }
              | type_ ',' types_     { $1 : $3 }

rowType       : ':' type_            { $2 }

rowTypes_     : rowType                { [$1] }
              | rowType ';' rowTypes_  { $1 : $3 }


tupTypes : rowType              { [$1] }
         | rowType ',' tupTypes { $1 : $3 }


ptrType : type_                 { $1 }
        | null                  { T.Void }
        | ident ':' type_       { T.Named (tokStr $1) $3 }
        | ident ':' null        { T.Named (tokStr $1) T.Void }
ptrTypes : ptrType              { [$1] }
         | ptrType '|' ptrTypes { $1 : $3 }

pattern  : '_'                     { S.PatIgnore (tokPos $1) }
         | lit                     { S.PatLiteral $1 }
         | ident                   { S.PatIdent (tokPos $1) (tokStr $1) }
         | type_ '(' pattern ')'   { S.PatTyped (tokPos $2) $1 $3 }
         | '(' patterns ')'        { S.PatTuple (tokPos $1) $2 }
         | '[' patterns ']'        { S.PatArray (tokPos $1) $2 }
         | pattern '|' expr        { S.PatGuarded (tokPos $2) $1 $3 }


patterns  : {- empty -}             { [] }
          | patterns_               { $1 }
patterns_ : pattern                 { [$1] }
          | pattern ',' patterns_   { $1 : $3 }


param   : ident type_               { S.Param (tokPos $1) (tokStr $1) $2 }
params  : {- empty -}               { [] }
        | params_                   { $1 }
params_ : param                     { [$1] }
        | param ',' params_         { $1 : $3 }


index  : ident                      { S.IndIdent (tokPos $1) (tokStr $1) }
       | index '[' expr ']'         { S.IndArray (tokPos $2) $1 $3 }
       | index '.' intlit           { S.IndTuple (tokPos $2) $1 (read $ tokStr $3) }


Switch : switch expr 'I' cases 'D'  { S.Switch (tokPos $1) $2 $4 }
cases  : {- empty -}                { [] }
       | cases_                     { $1 }
cases_ : case                       { [$1] }
       | case 'N' cases_            { $1 : $3 }
case   : pattern ';' stmtS          { ($1, $3) }


If    : if expr block                { S.If (tokPos $1) $2 $3 Nothing }
      | if expr block else_          { S.If (tokPos $1) $2 $3 (Just $4) }
else_ : else block                   { $2 }
      | else If                      { $2 }



{
parse :: MonadError Error m => Int -> String -> m S.AST
parse id source = do
    case alexScanner id source of
        Left  errStr -> throwError (ErrorStr errStr)
        Right tokens -> case (parseTokens tokens) 0 of
            ParseFail pos -> throwError (ErrorFile "" pos "parse error")
            ParseOk ast   -> return ast 


data ParseResult a
    = ParseOk a 
    | ParseFail TextPos
    deriving (Show)


type P a = Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
thenP m k = \l -> case m l of
    ParseFail s -> ParseFail s
    ParseOk a -> k a l


returnP :: a -> P a
returnP a = \l -> ParseOk a

tokPos :: Token -> TextPos
tokPos tok = tokPosn tok


happyError :: [Token] -> P a
happyError []    = return $ ParseFail (TextPos (-1) 0 0 0)
happyError (x:_) = return $ ParseFail (tokPosn x)

}
