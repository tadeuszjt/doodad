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
%right     '..'
%left      '?'
%left      '<<-' '<-'
%right     '->>' '->'
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
    '!'        { Token _ ReservedOp "!" }
    '!='       { Token _ ReservedOp "!=" }
    '<='       { Token _ ReservedOp "<=" }
    '>='       { Token _ ReservedOp ">=" }
    '=='       { Token _ ReservedOp "==" }
    '&&'       { Token _ ReservedOp "&&" }
    '||'       { Token _ ReservedOp "||" }
    '&'        { Token _ ReservedOp "&" }
    '?'        { Token _ ReservedOp "!" }
    '<-'       { Token _ ReservedOp "<-" }
    '->'       { Token _ ReservedOp "->" }
    '..'       { Token _ ReservedOp ".." }
    '<<-'      { Token _ ReservedOp "<<-" }
    '->>'      { Token _ ReservedOp "->>" }
    '#'        { Token _ ReservedOp "#" }

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
    import     { Token _ Reserved "import" }
    copy       { Token _ Reserved "copy" }
    for        { Token _ Reserved "for" }

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

---------------------------------------------------------------------------------------------------
-- Header -----------------------------------------------------------------------------------------

prog  : prog_                                 { S.AST Nothing [] $1 }
      | module ident 'N' imports prog_        { S.AST (Just (tokStr $2)) $4 $5 }
prog_ : {-empty-}                             { [] }
      | stmtS 'N' prog_                       { $1 : $3 }
      | stmtB prog_                           { $1 : $2 }

imports : {- empty -}                         { [] }
        | import importPath 'N' imports       { $2 : $4 }

importPath : ident                            { [tokStr $1] }
           | '..' '/' importPath              { ".." : $3 }
           | ident '/' importPath             { (tokStr $1) : $3 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------

stmtS : let pattern '=' expr                  { S.Assign (tokPos $1) $2 $4 }  
      | index '=' expr                        { S.Set (tokPos $2) $1 $3 }
      | index '(' exprs ')'                   { S.CallStmt (tokPos $2) $1 $3 }
      | type ident type_                      { S.Typedef (tokPos $2) (tokStr $2) $3 }
      | extern ident '(' params ')' type_     { S.Extern (tokPos $2) (tokStr $2) $4 $6 }
      | extern ident '(' params ')'           { S.Extern (tokPos $2) (tokStr $2) $4 T.Void }
      | print '(' exprs ')'                   { S.Print (tokPos $1) $3 }
      | return                                { S.Return (tokPos $1) Nothing }
      | return expr                           { S.Return (tokPos $1) (Just $2) }
      | append_                               { S.AppendStmt $1 }
stmtB : If                                    { $1 }
      | fn fnName '(' params ')' block        { S.FuncDef (tokPos $1) $2 $4 T.Void $6 }
      | fn fnName '(' params ')' type_ block  { S.FuncDef (tokPos $1) $2 $4 $6 $7 }
      | switch expr switchBlock               { S.Switch (tokPos $1) $2 $3 }
      | while condition block                 { S.While (tokPos $1) $2 $3 }
      | for '[' ident ']' expr block          { S.For (tokPos $1) (tokStr $3) $5 Nothing $6 }
      | for '[' ident ']' expr '|' expr block { S.For (tokPos $1) (tokStr $3) $5 (Just $7) $8 }

pattern  : '_'                                { S.PatIgnore (tokPos $1) }
         | literal                            { S.PatLiteral $1 }
         | ident                              { S.PatIdent (tokPos $1) (tokStr $1) }
         | typeOrdinal '(' patterns ')'       { S.PatTyped (tokPos $2) $1 $3 }
         | ident '(' patterns ')'             { S.PatTyped (tokPos $2) (T.Typedef $ T.Sym $ tokStr $1) $3 }
         | '(' patterns ')'                   { S.PatTuple (tokPos $1) $2 }
         | '[' patterns ']'                   { S.PatArray (tokPos $1) $2 }
         | pattern '->' pattern               { S.PatSplitElem (tokPos $2) $1 $3 }
         | pattern '->>' pattern              { S.PatSplit (tokPos $2) $1 $3 }
         | pattern '|' expr                   { S.PatGuarded (tokPos $2) $1 $3 }

index  : ident                                { S.IndIdent (tokPos $1) (tokStr $1) }
       | index '[' expr ']'                   { S.IndArray (tokPos $2) $1 $3 }
       | index '.' ident                      { S.IndTuple (tokPos $2) $1 (read $ tokStr $3) }

append_ : append_ '<<-' expr                  { S.AppendTable (tokPos $2) $1 $3 }
        | append_ '<-' expr                   { S.AppendElem (tokPos $2) $1 $3 }
        | index                               { S.AppendIndex $1 }

fnName  : ident                               { tokStr $1 }
        | string                              { tokStr $1 }
        | '+'                                 { tokStr $1 }
        | '-'                                 { tokStr $1 }
        | '*'                                 { tokStr $1 }
        | '/'                                 { tokStr $1 }
        | '%'                                 { tokStr $1 }
        | '<'                                 { tokStr $1 }
        | '>'                                 { tokStr $1 }
        | '='                                 { tokStr $1 }
        | '!'                                 { tokStr $1 }
        | '!='                                { tokStr $1 }
        | '<='                                { tokStr $1 }
        | '>='                                { tokStr $1 }
        | '=='                                { tokStr $1 }
        | '&&'                                { tokStr $1 }
        | '||'                                { tokStr $1 }

block  : 'I' prog_ 'D'                        { S.Block $2 }
       | ';' stmtS 'N'                        { S.Block [$2] }
--       | ';' stmtB                            { S.Block [$2] }
       | ';' 'N'                              { S.Block [] }

switchBlock : 'I' cases 'D'                   { $2 }
            | ';' case                        { [$2] }
            | ';' 'N'                         { [] }
cases  : case cases                           { ($1:$2) }
       | {-empty-}                            { [] }
case   : pattern block                        { ($1, $2) }


condition : expr                              { S.CondExpr $1 }
          | expr '#' pattern                  { S.CondMatch $3 $1 }

patterns  : {- empty -}                       { [] }
          | patterns_                         { $1 }
patterns_ : pattern                           { [$1] }
          | pattern ',' patterns_             { $1 : $3 }

param   : ident type_                         { S.Param (tokPos $1) (tokStr $1) $2 }
params  : {- empty -}                         { [] }
        | params_                             { $1 }
params_ : param                               { [$1] }
        | param ',' params_                   { $1 : $3 }

If    : if condition block else_              { S.If (tokPos $1) $2 $3 $4 }
      | if condition 'N' else_                { S.If (tokPos $1) $2 (S.Block []) $4 }
else_ : else block                            { Just $2 }
      | else If                               { Just $2 }
      | {-empty-}                             { Nothing }

---------------------------------------------------------------------------------------------------
-- Expressions ------------------------------------------------------------------------------------

expr   : literal                              { $1 }
       | infix                                { $1 }
       | prefix                               { $1 }
       | ident                                { S.Ident (tokPos $1) (tokStr $1) }
       | '[' tableRows ']'                    { S.Table (tokPos $1) $2 }
       | '[' 'I' exprsN 'D' ']'               { S.Table (tokPos $1) [$3] }
       | '[' '|' exprs ']'                    { S.Array (tokPos $1) $3 }
       | '(' exprs ')'                        { S.Tuple (tokPos $1) $2 }
       | expr '(' exprs ')'                   { S.Call (tokPos $2) $1 $3 }
       | len '(' expr ')'                     { S.Len (tokPos $1) $3 }
       | copy '(' expr ')'                    { S.Copy (tokPos $1) $3 }
       | typeOrdinal '(' exprs ')'            { S.Conv (tokPos $2) $1 $3 }
       | ':' typeAggregate '(' exprs ')'      { S.Conv (tokPos $3) $2 $4 }
       | expr '.' intlit                      { S.TupleIndex (tokPos $2) $1 (read $ tokStr $3) }
       | expr '.' ident                       { S.Member (tokPos $2) $1 (tokStr $3) }
       | expr '[' expr ']'                    { S.Subscript (tokPos $2) $1 $3 }
       | expr '[' expr '..' ']'               { S.Range (tokPos $2) $1 (Just $3) Nothing }
       | expr '[' '..' expr ']'               { S.Range (tokPos $2) $1 Nothing (Just $4) }
       | expr '[' expr  '..' expr ']'         { S.Range (tokPos $2) $1 (Just $3) (Just $5) }

literal : intlit                              { S.Int (tokPos $1) (read $ tokStr $1) }
        | floatlit                            { S.Float (tokPos $1) (read $ tokStr $1) }
        | charlit                             { S.Char (tokPos $1) (read $ tokStr $1) }
        | strlit                              { S.String (tokPos $1) (tokStr $1) }
        | true                                { S.Bool (tokPos $1) True }
        | false                               { S.Bool (tokPos $1) False }
        | null                                { S.Null (tokPos $1) }

infix : expr '+' expr                         { S.Infix (tokPos $2) S.Plus $1 $3 }
      | expr '-' expr                         { S.Infix (tokPos $2) S.Minus $1 $3 }
      | expr '*' expr                         { S.Infix (tokPos $2) S.Times $1 $3 }
      | expr '/' expr                         { S.Infix (tokPos $2) S.Divide $1 $3 }
      | expr '%' expr                         { S.Infix (tokPos $2) S.Modulo $1 $3 }
      | expr '<' expr                         { S.Infix (tokPos $2) S.LT $1 $3 }
      | expr '>' expr                         { S.Infix (tokPos $2) S.GT $1 $3 }
      | expr '<=' expr                        { S.Infix (tokPos $2) S.LTEq $1 $3 }
      | expr '>=' expr                        { S.Infix (tokPos $2) S.GTEq $1 $3 }
      | expr '==' expr                        { S.Infix (tokPos $2) S.EqEq $1 $3 }
      | expr '&&' expr                        { S.Infix (tokPos $2) S.AndAnd $1 $3 }
      | expr '||' expr                        { S.Infix (tokPos $2) S.OrOr $1 $3 }
      | expr '!=' expr                        { S.Infix (tokPos $2) S.NotEq $1 $3 }

exprs  : {- empty -}                          { [] }
       | exprs_                               { $1 }
exprs_ : expr                                 { [$1] }
       | expr ',' exprs_                      { $1 : $3 }

exprsN : expr                                 { [$1] }
       | expr ',' 'N' exprsN                  { $1 : $4 }

tableRows : exprs                             { [$1] }
          | exprs ';' tableRows               { $1 : $3 }

prefix : '-' expr                             { S.Prefix (tokPos $1) S.Minus $2 }
       | '+' expr                             { S.Prefix (tokPos $1) S.Plus $2 }
       | '!' expr                             { S.Prefix (tokPos $1) S.Not $2 }


---------------------------------------------------------------------------------------------------
-- Types ------------------------------------------------------------------------------------------

type_         : ident                         { T.Typedef (T.Sym $ tokStr $1) }
              | ident '.' ident               { T.Typedef (T.SymQualified (tokStr $1) (tokStr $3)) }
              | typeOrdinal                   { $1 }
              | typeAggregate                 { $1 }

typeOrdinal   : bool                          { T.Bool }
              | i16                           { T.I16 }
              | i32                           { T.I32 }
              | i64                           { T.I64 }
              | f32                           { T.F32 }
              | f64                           { T.F64 }
              | char                          { T.Char }
              | string                        { T.Table [T.Char] }

typeAggregate : '[' intlit ':' type_ ']'      { T.Array (read $ tokStr $2) $4 }
              | '(' tupTypes ')'              { T.Tuple $2 }
              | '[' rowTypes_ ']'             { T.Table $2 }
              | '{' adtTypes '}'              { T.ADT $2 }
              | '{' 'I' adtTypes 'D' '}'      { T.ADT $3 }
              | fn '(' argTypes ')' type_     { T.Func $3 $5 }

argTypes  : {-empty -}                        { [] }
          | argTypes_                         { $1 }
argTypes_ : type_                             { [$1] }
          | type_ ',' argTypes_               { $1:$3 }

tupTypes  : {-empty -}                        { [] }
          | tupTypes_                         { $1 }
tupTypes_ : tupType                           { [$1] }
          | tupType ',' tupTypes              { $1 : $3 }
tupType   : type_                             { ("", $1) }
          | ident ':' type_                   { (tokStr $1, $3) }

rowTypes_     : type_                         { [$1] }
              | type_ ';' rowTypes_           { $1 : $3 }

adtType : null                                { ("", T.Void) }
        | type_                               { ("", $1) }
        | ident '(' tupTypes ')'              { (tokStr $1, case length $3 of; 0 -> T.Void; 1 -> snd (head $3); n -> T.Tuple $3) }

adtTypes : adtType 'N'                        { [$1] }
         | adtType '|' adtTypes               { $1 : $3 }
         | adtType 'N' adtTypes               { $1 : $3 }

{
parse :: MonadError Error m => Int -> String -> m S.AST
parse id source = do
    case alexScanner id source of
        Left  errStr -> throwError (ErrorStr errStr)
        Right tokens -> case (parseTokens tokens) 0 of
            ParseFail pos -> throwError (ErrorSrc source pos "parse error")
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
