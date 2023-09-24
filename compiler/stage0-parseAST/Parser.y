{
{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Token
import Error
import Control.Monad.Except hiding (void, fail)
import Data.Char
import qualified Type as T
import qualified AST as S
import qualified Data.Set as Set
import Symbol
}

%name      parseTokens 
%tokentype { Token }
%monad { P } { thenP } { returnP }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  '<=' '>=' '<' '>'
%right     '!'
%left      ':'
%nonassoc  '!'
%nonassoc  '->'
%nonassoc  '(' ')' '[' ']' '{' '}'
%nonassoc  '|'
%nonassoc  '.'



%token
    'I'        { Token _ Indent _ }
    'D'        { Token _ Dedent _ }
    'N'        { Token _ NewLine _ }

    '+'        { Token _ TokSym "+" }
    '-'        { Token _ TokSym "-" }
    '*'        { Token _ TokSym "*" }
    '/'        { Token _ TokSym "/" }
    '%'        { Token _ TokSym "%" }
    '<'        { Token _ TokSym "<" }
    '>'        { Token _ TokSym ">" }
    '='        { Token _ TokSym "=" }
    '!'        { Token _ TokSym "!" }
    '@'        { Token _ TokSym "@" }
    '~'        { Token _ TokSym "~" }
    '!='       { Token _ TokSym "!=" }
    '<='       { Token _ TokSym "<=" }
    '>='       { Token _ TokSym ">=" }
    '=='       { Token _ TokSym "==" }
    '&&'       { Token _ TokSym "&&" }
    '||'       { Token _ TokSym "||" }
    '::'       { Token _ TokSym "::" }
    '->'       { Token _ TokSym "->" }
    '..'       { Token _ TokSym ".." }
    '+='       { Token _ TokSym "+=" }

    fn         { Token _ Reserved "fn" }
    const      { Token _ Reserved "const" }
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
    for        { Token _ Reserved "for" }
    null       { Token _ Reserved "null" }
    data       { Token _ Reserved "data" }
    import     { Token _ Import _ }
    include    { Token _ CInclude _ }
    link       { Token _ CLink _ }

    u8         { Token _ Reserved "u8" }
    i8         { Token _ Reserved "i8" }
    i16        { Token _ Reserved "i16" }
    i32        { Token _ Reserved "i32" }
    i64        { Token _ Reserved "i64" }
    f32        { Token _ Reserved "f32" }
    f64        { Token _ Reserved "f64" }
    bool       { Token _ Reserved "bool" }
    char       { Token _ Reserved "char" }
    string     { Token _ Reserved "string" }
    table      { Token _ Reserved "table" }
    integer    { Token _ Reserved "integer" }

    int_c      { Token _ Int _ }
    float_c    { Token _ Float _ }
    char_c     { Token _ Char _ }
    string_c   { Token _ String _ }
    ident      { Token _ Ident _ }

    embed_c    { Token _ EmbedC _ }

    '('        { Token _ TokSym "(" }
    ')'        { Token _ TokSym ")" }
    '{'        { Token _ TokSym "{" }
    '}'        { Token _ TokSym "}" }
    '['        { Token _ TokSym "[" }
    ']'        { Token _ TokSym "]" }
    '|'        { Token _ TokSym "|" }
    ','        { Token _ TokSym "," }
    '.'        { Token _ TokSym "." }
    ';'        { Token _ TokSym ";" }
    ':'        { Token _ TokSym ":" }
    '_'        { Token _ TokSym "_" }
%%

---------------------------------------------------------------------------------------------------
-- Header -----------------------------------------------------------------------------------------

prog  : stmts                               { S.AST "main" [] $1 }
      | header stmts                        { S.AST (tokStr $ fst $1) (snd $1) $2 }
stmts : {-empty-}                           { [] }
      | line 'N' stmts                      { $1 : $3 }
      | block stmts                         { $1 : $2 }

header : module ident 'N' imports           { ($2, $4) }
imports : {- empty -}                       { [] }
        | import 'N' imports                { S.Import   (tokStr $1) : $3 }
        | include 'N' imports               { S.CInclude (tokStr $1) : $3 }
        | link 'N' imports                  { S.CLink (tokStr $1) : $3 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------

symbol : ident                              { (tokPos $1, Sym (tokStr $1)) }
       | ident '::' ident                   { (tokPos $3, SymQualified (tokStr $1) (tokStr $3)) }

symbols1 : symbol               { [$1] }
         | symbol ',' symbols1  { $1 : $3 }

mfnrec : {-empty-}                          { [] }
       | '{' paramsA '}'                    { $2 }


line : let pattern '=' expr                         { S.Assign (tokPos $1) $2 $4 }  
     | index '=' expr                               { S.SetOp (tokPos $2) S.Eq $1 $3 }
     | index '+=' expr                              { S.SetOp (tokPos $2) S.PlusEq $1 $3 }
     | index                                        { S.ExprStmt $1 }
     | type symbol anno_t                           { S.Typedef (fst $2) (snd $2) $3 }
     | data symbol type_                            { S.Data (tokPos $1) (snd $2) $3 Nothing }
     --| data symbol type_ '=' mexpr                      { S.Data (tokPos $1) (snd $2) $3 $4 }
     | return mexpr                                 { S.Return (tokPos $1) $2 }
     | embed_c                                      { S.EmbedC (tokPos $1) (tokStr $1) }
     | const symbol '=' expr                        { S.Const (tokPos $1) (snd $2) $4 }
block : if_                                         { $1 }
      | fn mfnrec ident '(' paramsA ')' mtype scope {
            S.FuncDef
                (tokPos $1)
                $2
                (Sym $ tokStr $3)
                $5
                (case $7 of Just t -> t; Nothing -> T.Void) $8
        }
      | while condition scope                       { S.While (tokPos $1) $2 $3 }
      | for expr scope                              { S.For (tokPos $1) $2 Nothing $3 }
      | for expr '->' pattern scope                 { S.For (tokPos $1) $2 (Just $4) $5 }
      | switch_                                     { $1 }

scope  : 'I' stmts 'D'                      { S.Block $2 }
       | ';' line 'N'                       { $2 }
       | ';' 'N'                            { S.Block [] }

condition : expr                            { $1 }
          | expr '->' pattern               { S.Match (tokPos $2) $1 $3 }


param   : ident ':' type_                   { S.Param (tokPos $1) (Sym $ tokStr $ $1) $3 }
params  : {- empty -}                       { [] }
        | params1                           { $1 }
params1 : param                             { [$1] }
        | param ',' params1                 { $1 : $3 }
params2 : param  ',' params1                { $1 : $3 }

paramL  : ident ':' type_                   { S.Param (tokPos $1) (Sym $ tokStr $ $1) $3 }
        | ident ':' null                    { S.Param (tokPos $1) (Sym $ tokStr $ $1) T.Void }

paramL_ : paramL { $1 }
         | paramL 'N' {$1}
paramsL1 : paramL_                          { [$1] }
         | paramL_ '|' paramsL1             { $1 : $3 }
paramsL2 : paramL_ '|' paramsL1             { $1 : $3 }

paramsN : param 'N'                         { [$1] } 
        | param 'N' paramsN                 { $1 : $3 }
paramsA : params                            { $1 }
        | 'I' paramsN 'D'                   { $2 }


if_   : if condition scope else_            { S.If (tokPos $1) $2 $3 $4 }
      | if condition 'N' else_              { S.If (tokPos $1) $2 (S.Block []) $4 }
else_ : else scope                          { Just $2 }
      | else if_                            { Just $2 }
      | {-empty-}                           { Nothing }


switch_ : switch expr 'I' cases1 'D'        { S.Switch (tokPos $1) $2 $4 }
       | switch expr                        { S.Switch (tokPos $1) $2 [] }
cases1 : case                               { [$1] }
      | case cases1                         { $1 : $2 }
case : pattern scope                        { ($1, $2) }

---------------------------------------------------------------------------------------------------
-- Patterns ---------------------------------------------------------------------------------------

patterns  : {- empty -}                     { [] }
          | patterns1                       { $1 }
patterns1 : pattern                         { [$1] }
          | pattern ',' patterns1           { $1 : $3 }

pattern  : '_'                              { S.PatIgnore (tokPos $1) }
         | literal                          { S.PatLiteral $1 }
         | '-' int_c                        { S.PatLiteral (S.Int (tokPos $1) $ 0 - (read $ tokStr $2)) }
         | ident                            { S.PatIdent (tokPos $1) (Sym $ tokStr $1) }
         | null                             { S.PatNull (tokPos $1) }
         | '(' patterns ')'                 { S.PatTuple (tokPos $1) $2 }
         | '[' patterns ']'                 { S.PatArray (tokPos $1) $2 }
         | pattern '|' expr                 { S.PatGuarded (tokPos $2) $1 $3 }
         | pattern '|' expr '->' pattern    { S.PatGuarded (tokPos $2) $1 (S.Match (tokPos $4) $3 $5) }
         | symbol '(' patterns ')'          { S.PatField (tokPos $2) (snd $1) $3 }
         | pattern '[' type_ ']'            { S.PatTypeField (tokPos $2) $3 $1 }
         | pattern ':' type_                { S.PatAnnotated $1 $3 }

---------------------------------------------------------------------------------------------------
-- Expressions ------------------------------------------------------------------------------------

exprs  : {- empty -}                        { [] }
       | exprs1                             { $1 }
exprs1 : expr                               { [$1] }
       | expr ',' exprs1                    { $1 : $3 }
exprsN : expr 'N'                           { [$1] } 
       | expr 'N' exprsN                    { $1 : $3 }
mexpr  : {-empty-}                          { Nothing }
       | expr                               { Just $1 }
exprsA : exprs                              { $1 }
       | 'I' exprsN 'D'                     { $2 }

call : symbol '(' exprsA ')'                   { S.Call (tokPos $2) [] (snd $1) $3 }
     | '{' exprsA '}' '.' ident '(' exprsA ')' { S.Call (tokPos $4) $2 (Sym $ tokStr $5) $7 }

index  : symbol                             { S.Ident (fst $1) (snd $1) }
       | index '[' expr ']'                 { S.Subscript (tokPos $2) $1 $3 }
       | index '.' ident                    { S.Field (tokPos $2) $1 (Sym $ tokStr $3) }
       | index '.' symbol '(' exprsA ')'    { S.Call (tokPos $2) [$1] (snd $3) $5 }
       | call                               { $1 }

expr   : literal                            { $1 }
       | infix                              { $1 }
       | prefix                             { $1 }
       | call                               { $1 }
       | symbol                             { S.Ident (fst $1) (snd $1) }
       | '(' exprsA ')'                     { case $2 of [x] -> x; xs -> S.Tuple (tokPos $1) xs }
       | ordinal_t '(' exprsA ')'           { S.Conv (tokPos $2) $1 $3 }
       | null                               { S.Null (tokPos $1) }
       | expr '.' ident                     { S.Field (tokPos $2) $1 (Sym $ tokStr $3) }
       | expr '[' expr ']'                  { S.Subscript (tokPos $2) $1 $3 }
       | expr ':' type_                     { S.AExpr $3 $1 }
       | expr '.' ident '(' exprsA ')'      { S.Call (tokPos $4) [$1] (Sym $ tokStr $3) $5 }
       | expr '[' mexpr '..' mexpr ']'      { S.Range (tokPos $2) (Just $1) $3 $5 }
       | '[' mexpr '..' mexpr ']'           { S.Range (tokPos $1) Nothing $2 $4 }
       | '[' exprsA ']'                     { S.Array (tokPos $1) $2 }

literal : int_c                             { S.Int (tokPos $1) (read $ tokStr $1) }
        | float_c                           { S.Float (tokPos $1) (read $ tokStr $1) }
        | char_c                            { S.Char (tokPos $1) (read $ tokStr $1) }
        | string_c                          { S.String (tokPos $1) (tokStr $1) }
        | true                              { S.Bool (tokPos $1) True }
        | false                             { S.Bool (tokPos $1) False }

infix : expr '+' expr                       { S.Infix (tokPos $2) S.Plus $1 $3 }
      | expr '-' expr                       { S.Infix (tokPos $2) S.Minus $1 $3 }
      | expr '*' expr                       { S.Infix (tokPos $2) S.Times $1 $3 }
      | expr '/' expr                       { S.Infix (tokPos $2) S.Divide $1 $3 }
      | expr '%' expr                       { S.Infix (tokPos $2) S.Modulo $1 $3 }
      | expr '<' expr                       { S.Infix (tokPos $2) S.LT $1 $3 }
      | expr '>' expr                       { S.Infix (tokPos $2) S.GT $1 $3 }
      | expr '<=' expr                      { S.Infix (tokPos $2) S.LTEq $1 $3 }
      | expr '>=' expr                      { S.Infix (tokPos $2) S.GTEq $1 $3 }
      | expr '==' expr                      { S.Infix (tokPos $2) S.EqEq $1 $3 }
      | expr '&&' expr                      { S.Infix (tokPos $2) S.AndAnd $1 $3 }
      | expr '||' expr                      { S.Infix (tokPos $2) S.OrOr $1 $3 }
      | expr '!=' expr                      { S.Infix (tokPos $2) S.NotEq $1 $3 }

prefix : '-' expr                           { S.Prefix (tokPos $1) S.Minus $2 }
       | '+' expr                           { S.Prefix (tokPos $1) S.Plus $2 }
       | '!' expr                           { S.Prefix (tokPos $1) S.Not $2 }

---------------------------------------------------------------------------------------------------
-- Types ------------------------------------------------------------------------------------------
mtype  : {-empty-}                          { Nothing }
       | type_                              { Just $1 }
types1 : type_                              { [$1] }
       | type_ ',' types1                   { $1 : $3 }
    
type_         : symbol                      { T.Typedef (snd $1) }
              | ordinal_t                   { $1 }
              | aggregate_t                 { $1 }
              | symbol '(' types1 ')'       { T.TypeApply (snd $1) $3 }

uncovered_t : symbol { T.Typedef (snd $1) }
            | ordinal_t { $1 }
            | array_t   { $1 }
            | table_t   { $1 }
            | range_t   { $1 }
            | tuple_t   { $1 }
            | adtFields2 { T.ADT $1 }

uncovered_ts1 : uncovered_t                    { [$1] }
              | uncovered_t  ',' uncovered_ts1 { $1 : $3 }
uncovered_ts2 : uncovered_t  ',' uncovered_ts1 { $1 : $3 }

ordinal_t   : bool                          { T.Bool }
            | u8                            { T.U8 }
            | i8                            { T.I8 }
            | i16                           { T.I16 }
            | i32                           { T.I32 }
            | i64                           { T.I64 }
            | f32                           { T.F32 }
            | f64                           { T.F64 }
            | char                          { T.Char }
            | string                        { T.String }

aggregate_t : table_t                       { $1 }
            | array_t                     { $1 }
            | tuple_t                     { $1 }
            | adt_t                       { $1 }
            | range_t                     { $1 }


adt_t    : '(' adtField '|' adtFields1 ')'         { T.ADT ($2:$4) }
array_t  : '[' int_c uncovered_t ']'               { T.Array (read $ tokStr $2) $3 }
table_t  : '[' uncovered_ts1 ']'                   { T.Table $2 }
tuple_t  : '(' uncovered_ts2 ')'                   { T.Tuple $2 }
         | '(' ')'                                 { T.Tuple [] }
range_t  : '[' '..' ']' type_                      { T.Range $4 }

anno_t   : ordinal_t                        { S.AnnoType $1 }
         | symbol                           { S.AnnoType (T.Typedef $ snd $1) }
         | '(' uncovered_ts1 ')'            { S.AnnoType (T.Tuple $2) }
         | '(' params2 ')'                  { S.AnnoTuple $2 }
         | '(' paramsL2 ')'                 { S.AnnoADT (map paramToAdtField $2) }
         | '(' 'I' paramsL2 'D' ')'         { S.AnnoADT (map paramToAdtField $3) }
         | '[' params1 ']'                  { S.AnnoTable $2 }
         | array_t                          { S.AnnoType $1 }
         | table_t                          { S.AnnoType $1 }


adtFields1 : adtField                       { [$1] }
           | adtField '|' adtFields1        { $1 : $3 }


adtFields2 : adtField '|' adtFields1        { $1 : $3 }
adtField : type_                            { T.FieldType $1 }
         | null                             { T.FieldNull }

{

paramToAdtField :: S.Param -> S.AnnoADTField
paramToAdtField (S.Param pos symbol typ) = case typ of
    T.Tuple [] -> S.ADTFieldMember symbol []
    T.Tuple ts -> S.ADTFieldMember symbol ts
    t          -> S.ADTFieldMember symbol [t]


parse :: MonadError Error m => [Token] -> m S.AST
parse tokens = do
    case (parseTokens tokens) 0 of
        ParseFail pos -> throwError (ErrorPos pos "parse error")
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
happyError []    = return $ ParseFail (TextPos "" 0 0)
happyError (x:_) = return $ ParseFail (tokPosn x)

}
