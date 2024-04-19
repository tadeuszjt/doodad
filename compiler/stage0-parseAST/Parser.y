{
{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Token
import Error
import Control.Monad.Except hiding (void, fail)
import Type
import AST
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
%nonassoc  '<=' '>=' '<' '>' '++'
%right     '&' '*'
%right     '!'
%left      ':'
%nonassoc  '!'
%nonassoc  '->'
%nonassoc  '(' ')' '[' ']' '{' '}'
%nonassoc  '|'
%nonassoc  '.'


%token
    'I'        { Token _ Token.Indent _ }
    'D'        { Token _ Token.Dedent _ }
    'N'        { Token _ Token.NewLine _ }

    '+'        { Token _ Token.TokSym "+" }
    '-'        { Token _ Token.TokSym "-" }
    '*'        { Token _ Token.TokSym "*" }
    '/'        { Token _ Token.TokSym "/" }
    '%'        { Token _ Token.TokSym "%" }
    '<'        { Token _ Token.TokSym "<" }
    '>'        { Token _ Token.TokSym ">" }
    '='        { Token _ Token.TokSym "=" }
    '!'        { Token _ Token.TokSym "!" }
    '@'        { Token _ Token.TokSym "@" }
    '~'        { Token _ Token.TokSym "~" }
    '#'        { Token _ Token.TokSym "#" }
    '&'        { Token _ Token.TokSym "&" }
    '!='       { Token _ Token.TokSym "!=" }
    '<='       { Token _ Token.TokSym "<=" }
    '>='       { Token _ Token.TokSym ">=" }
    '=='       { Token _ Token.TokSym "==" }
    '&&'       { Token _ Token.TokSym "&&" }
    '||'       { Token _ Token.TokSym "||" }
    '::'       { Token _ Token.TokSym "::" }
    '->'       { Token _ Token.TokSym "->" }
    '..'       { Token _ Token.TokSym ".." }
    '+='       { Token _ Token.TokSym "+=" }
    '++'       { Token _ Token.TokSym "++" }

    fn         { Token _ Token.Reserved "fn" }
    const      { Token _ Token.Reserved "const" }
    type       { Token _ Token.Reserved "type" }
    if         { Token _ Token.Reserved "if" }
    else       { Token _ Token.Reserved "else" }
    let        { Token _ Token.Reserved "let" }
    in         { Token _ Token.Reserved "in" }
    while      { Token _ Token.Reserved "while" }
    return     { Token _ Token.Reserved "return" }
    switch     { Token _ Token.Reserved "switch" }
    true       { Token _ Token.Reserved "true" }
    false      { Token _ Token.Reserved "false" }
    for        { Token _ Token.Reserved "for" }
    data       { Token _ Token.Reserved "data" }
    module     { Token _ Token.Module _ }
    import     { Token _ Token.Import _ }
    include    { Token _ Token.CInclude _ }
    link       { Token _ Token.CLink _ }

    U8         { Token _ Token.Reserved "U8" }
    I8         { Token _ Token.Reserved "I8" }
    I16        { Token _ Token.Reserved "I16" }
    I32        { Token _ Token.Reserved "I32" }
    I64        { Token _ Token.Reserved "I64" }
    F32        { Token _ Token.Reserved "F32" }
    F64        { Token _ Token.Reserved "F64" }
    Bool       { Token _ Token.Reserved "Bool" }
    Char       { Token _ Token.Reserved "Char" }
    String     { Token _ Token.Reserved "String" }
    Table      { Token _ Token.Reserved "Table" }

    int_c      { Token _ Token.Int _ }
    float_c    { Token _ Token.Float _ }
    char_c     { Token _ Token.Char _ }
    string_c   { Token _ Token.String _ }
    ident      { Token _ Token.Ident _ }
    Ident      { Token _ Token.UpperIdent _ }

    embed_c    { Token _ Token.EmbedC _ }

    '('        { Token _ Token.TokSym "(" }
    ')'        { Token _ Token.TokSym ")" }
    '{'        { Token _ Token.TokSym "{" }
    '}'        { Token _ Token.TokSym "}" }
    '['        { Token _ Token.TokSym "[" }
    ']'        { Token _ Token.TokSym "]" }
    '|'        { Token _ Token.TokSym "|" }
    ','        { Token _ Token.TokSym "," }
    '.'        { Token _ Token.TokSym "." }
    ';'        { Token _ Token.TokSym ";" }
    ':'        { Token _ Token.TokSym ":" }
    '_'        { Token _ Token.TokSym "_" }
%%

---------------------------------------------------------------------------------------------------
-- Header -----------------------------------------------------------------------------------------

prog  : stmts                      { AST "main" [] $1 }
      | header stmts               { AST (tokStr $ fst $1) (snd $1) $2 }
stmts : {-empty-}                  { [] }
      | line 'N' stmts             { $1 : $3 }
      | block stmts                { $1 : $2 }

header : module 'N' imports        { ($1, $3) }
imports : {- empty -}              { [] }
        | import 'N' imports       { AST.Import   (tokStr $1) : $3 }
        | include 'N' imports      { AST.CInclude (tokStr $1) : $3 }
        | link 'N' imports         { AST.CLink (tokStr $1) : $3 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------


generics : {-empty-}                      { [] }
         | '[' Idents1 ']'                { map Symbol.Sym $2 }

retty : {-empty-} { VoidRetty }
      | type_     { Retty $1 }
      | '&' type_ { RefRetty $2 }

line : let pattern '=' expr               { Let (tokPos $1) $2 (Just $4) Nothing }  
     | let pattern                        { Let (tokPos $1) $2 Nothing Nothing }
     | expr                               { ExprStmt $1 }
     | expr '=' expr                      { SetOp (tokPos $2) Eq $1 $3 }
     | expr '+=' expr                     { SetOp (tokPos $2) PlusEq $1 $3 }
     | type generics Symbol anno_t        { Typedef (fst $3) $2 (snd $3) $4 }
     | data symbol type_                  { Data (tokPos $1) (snd $2) $3 Nothing }
     | return mexpr                       { Return (tokPos $1) $2 }
     | embed_c                            { AST.EmbedC (tokPos $1) (tokStr $1) }

block : if_                               { $1 }
      | while condition scope             { While (tokPos $1) $2 $3 }
      | type generics Symbol 'I' paramsLN1 'D' { Typedef (fst $3) $2 (snd $3) (AnnoADT $5) }
      | for expr scope                    { For (tokPos $1) $2 Nothing $3 }
      | for expr '->' pattern scope       { For (tokPos $1) $2 (Just $4) $5 }
      | switch expr 'I' cases1 'D'        { Switch (tokPos $1) $2 $4 }
      | let pattern '='  expr in scope    { Let (tokPos $1) $2 (Just $4) (Just $6) }
      | let pattern in scope              { Let (tokPos $1) $2 Nothing (Just $4) }
      | fn generics ident '(' paramsA ')' retty scope {
            FuncDef (tokPos $1) $2 [] (Sym $ tokStr $3) $5 $7 $8
        }

if_   : if condition scope else_          { If (tokPos $1) $2 $3 $4 }
      | if condition 'N' else_            { If (tokPos $1) $2 (Block []) $4 }
else_ : else scope                        { Just $2 }
      | else if_                          { Just $2 }
      | {-empty-}                         { Nothing }

scope  : 'I' stmts 'D'                    { Block $2 }
       | ';' line 'N'                     { $2 }
       | ';' 'N'                          { Block [] }

cases1 : case                             { [$1] }
       | case cases1                      { $1 : $2 }
case : pattern scope                      { ($1, $2) }


--------------------------------------------------------------------------------------------------
-- Misc ------------------------------------------------------------------------------------------

Idents1 : Ident                          { [tokStr $1] }
        | Ident ',' Idents1              { (tokStr $1):($3) } 

symbol : ident                           { (tokPos $1, Sym (tokStr $1)) }
       | ident '::' ident                { (tokPos $3, SymQualified (tokStr $1) (tokStr $3)) }

Symbol : Ident                           { (tokPos $1, Sym (tokStr $1)) }
       | ident '::' Ident                { (tokPos $3, SymQualified (tokStr $1) (tokStr $3)) }


param   : ident type_                    { Param (tokPos $1) (Sym $ tokStr $1) $2 }
        | ident '&' type_                { RefParam (tokPos $1) (Sym $ tokStr $1) $3 }
params  : {- empty -}                    { [] }
        | params1                        { $1 }
params1 : param                          { [$1] }
        | param ',' params1              { $1 : $3 }


paramL  : Ident type_                    { Param (tokPos $1) (Sym $ tokStr $1) $2 }
        | Ident '(' ')'                  { Param (tokPos $1) (Sym $ tokStr $1) Void }
paramsLN1 : paramL 'N'                   { [$1] }
          | paramL 'N' paramsLN1         { $1 : $3 }


paramsN : param 'N'                      { [$1] } 
        | param 'N' paramsN              { $1 : $3 }
paramsA : params                         { $1 }
        | 'I' paramsN 'D'                { $2 }
paramsA1 : params1                       { $1 }
         | 'I' paramsN 'D'               { $2 }


---------------------------------------------------------------------------------------------------
-- Patterns ---------------------------------------------------------------------------------------

patterns  : {- empty -}                  { [] }
          | patterns1                    { $1 }
patterns1 : pattern                      { [$1] }
          | pattern ',' patterns1        { $1 : $3 }

pattern  : '_'                           { PatIgnore (tokPos $1) }
         | literal                       { PatLiteral $1 }
         | '-' int_c                     { PatLiteral (AST.Int (tokPos $1) $ 0 - (read $ tokStr $2)) }
         | ident                         { PatIdent (tokPos $1) (Sym $ tokStr $1) }
         | '(' patterns ')'              { PatTuple (tokPos $1) $2 }
         | pattern '|' expr              { PatGuarded (tokPos $2) $1 $3 }
         | pattern '|' expr '->' pattern { PatGuarded (tokPos $2) $1 (Match (tokPos $4) $3 $5) }
         | Symbol '(' patterns ')'       { PatField (tokPos $2) (snd $1) $3 }
         | pattern ':' type_             { PatAnnotated $1 $3 }
 
---------------------------------------------------------------------------------------------------
-- Expressions ------------------------------------------------------------------------------------

exprs  : {- empty -}                             { [] }
       | exprs1                                  { $1 }
exprs1 : expr                                    { [$1] }
       | expr ',' exprs1                         { $1 : $3 }
exprsN : expr 'N'                                { [$1] } 
       | expr 'N' exprsN                         { $1 : $3 }
mexpr  : {-empty-}                               { Nothing }
       | expr                                    { Just $1 }
exprsA : exprs                                   { $1 }
       | 'I' exprsN 'D'                          { $2 }

condition : expr                                 { $1 }
          | expr '->' pattern                    { Match (tokPos $2) $1 $3 }

expr   : literal                                 { $1 }
       | infix                                   { $1 }
       | prefix                                  { $1 }
       | expr ':' type_                          { AExpr $3 $1 }
       | symbol                                  { AST.Ident (fst $1) (snd $1) }
       | '(' exprsA ')'                          { case $2 of [x] -> x; xs -> AST.Tuple (tokPos $1) xs }
       | symbol '(' exprsA ')'                   { Call (tokPos $2) Nothing (snd $1) $3 }
       | Symbol '(' exprsA ')'                   { Construct (tokPos $2) (snd $1) $3 }
       --| expr '.' symbol '(' exprsA ')'          { Call (tokPos $4) (Just $1) (snd $3) $5 }
       | expr '.' symbol '(' exprsA ')'          { Call (tokPos $4) Nothing (snd $3) (AST.Reference (tokPos $2) $1 : $5) }
       | expr '.' int_c                          { Field (tokPos $2) $1 (read $ tokStr $3)  }
       | expr '.' symbol                         { Call (tokPos $2) Nothing (snd $3) (AST.Reference (tokPos $2) $1 : []) }
--       | expr '.' ident                          { Field (tokPos $2) $1 (Sym $ tokStr $3) }
--       | expr '.' Ident                          { Field (tokPos $2) $1 (Sym $ tokStr $3) }
       | '&' expr                                { AST.Reference (tokPos $1) $2 }


literal : int_c                                  { AST.Int (tokPos $1) (read $ tokStr $1) }
        | float_c                                { AST.Float (tokPos $1) (read $ tokStr $1) }
        | char_c                                 { AST.Char (tokPos $1) (read $ tokStr $1) }
        | string_c                               { AST.String (tokPos $1) (tokStr $1) }
        | true                                   { AST.Bool (tokPos $1) True }
        | false                                  { AST.Bool (tokPos $1) False }

infix : expr '+' expr                            { Infix (tokPos $2) AST.Plus $1 $3 }
      | expr '-' expr                            { Infix (tokPos $2) AST.Minus $1 $3 }
      | expr '*' expr                            { Infix (tokPos $2) AST.Times $1 $3 }
      | expr '/' expr                            { Infix (tokPos $2) AST.Divide $1 $3 }
      | expr '%' expr                            { Infix (tokPos $2) AST.Modulo $1 $3 }
      | expr '<' expr                            { Infix (tokPos $2) AST.LT $1 $3 }
      | expr '>' expr                            { Infix (tokPos $2) AST.GT $1 $3 }
      | expr '<=' expr                           { Infix (tokPos $2) AST.LTEq $1 $3 }
      | expr '>=' expr                           { Infix (tokPos $2) AST.GTEq $1 $3 }
      | expr '==' expr                           { Infix (tokPos $2) AST.EqEq $1 $3 }
      | expr '&&' expr                           { Infix (tokPos $2) AST.AndAnd $1 $3 }
      | expr '||' expr                           { Infix (tokPos $2) AST.OrOr $1 $3 }
      | expr '!=' expr                           { Infix (tokPos $2) AST.NotEq $1 $3 }

prefix : '-' expr                                { Prefix (tokPos $1) Minus $2 }
       | '+' expr                                { Prefix (tokPos $1) Plus $2 }
       | '!' expr                                { Prefix (tokPos $1) Not $2 }

---------------------------------------------------------------------------------------------------
-- Types ------------------------------------------------------------------------------------------
mtype  : {-empty-}                         { Nothing }
       | type_                             { Just $1 }
types1 : type_                             { [$1] }
       | type_ ',' types1                  { $1 : $3 }
types1N : type_ 'N'                        { [$1] }
        | type_ ',' 'N' types1N            { $1 : $4 }
types2N : type_ ',' 'N' types1N            { $1 : $4 }
    
type_         : ordinal_t                  { $1 }
              | '(' type_ ')'              { $2 }
              | Symbol                     { TypeApply (snd $1) [] }
              | Symbol '[' types1 ']'      { TypeApply (snd $1) $3 }
              | tuple_t                    { $1 }
              | table_t                    { $1 }


ordinal_t   : Bool                         { Type.Bool }
            | U8                           { U8 }
            | I8                           { I8 }
            | I16                          { I16 }
            | I32                          { I32 }
            | I64                          { I64 }
            | F32                          { F32 }
            | F64                          { F64 }
            | Char                         { Type.Char }
            | String                       { Type.String }


tuple_t
    : '(' type_ ',' types1 ')' { Type.Tuple ($2:$4) }

table_t  : Table '[' type_ ']'             { Type.Table $3 }


anno_t   : ordinal_t                       { AnnoType $1 }
         | tuple_t                         { AnnoType $1 }
         | table_t                         { AnnoType $1 }
         | '(' ')' '{' paramsA1 '}'        { AnnoTuple $4 }
         | Table '[' '{' paramsA1 '}' ']'  { AnnoTable $4 }
         | '(' paramsA1 ')'                { AnnoTuple $2 }
{
parse :: MonadError Error m => [Token] -> m AST
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
