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
%left      '.'


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
    enum       { Token _ Token.Reserved "enum" }
    tuple      { Token _ Token.Reserved "tuple" }
    return     { Token _ Token.Reserved "return" }
    switch     { Token _ Token.Reserved "switch" }
    true       { Token _ Token.Reserved "true" }
    false      { Token _ Token.Reserved "false" }
    for        { Token _ Token.Reserved "for" }
    func       { Token _ Token.Reserved "func" }
    inst       { Token _ Token.Reserved "inst" }
    derives    { Token _ Token.Reserved "derives" }
    module     { Token _ Token.Module _ }
    import     { TokenImport _ _ _ _ _ }
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
        | import 'N' imports       { AST.Import (tokImpExp $1) (tokImpQual $1) (tokImpPath $1) (tokImpAs $1) : $3 }
        | include 'N' imports      { AST.CInclude (tokStr $1) : $3 }
        | link 'N' imports         { AST.CLink (tokStr $1) : $3 }


---------------------------------------------------------------------------------------------------
-- Macros -----------------------------------------------------------------------------------------

enumMacro : enum generics Ident '{'  'I' enumCases 'D' '}' { AST.Enum (tokPos $1) $2 (Sym [tokStr $3]) $6 }

enumCases : {-empty-}                         { [] }
          | ident 'N' enumCases               { (Sym [tokStr $1], []) : $3 }
          | ident '(' types ')' 'N' enumCases { (Sym [tokStr $1], $3) : $6 }


tupleMacro : tuple generics Ident '{' tupleFieldsN '}' { MacroTuple (tokPos $1) $2 (Sym [tokStr $3]) $5 }

tupleFieldsN : 'N'                   { [] }
             | 'I' tupleFields1 'D'  { $2 }

tupleFields1 : symbol type_ 'N'              { [(snd $1, $2)] }
             | symbol type_ 'N' tupleFields1 { (snd $1, $2) : $4 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------


derivesDef : derives generics type_ '(' callTypes1 ')'
           { Derives (tokPos $1) $2 $3 $5 }


funcGenerics : '{' Idents1 funDeps '}'  { (map (\s -> Symbol.Sym [s]) $2, $3) }
             | {-empty-}                { ([], []) }

funDeps : {-empty-}    { [] }
        | '|' funDeps1 { $2 }

funDep : Ident '->' Ident { (Symbol.Sym [tokStr $1], Symbol.Sym [tokStr $3]) }
funDeps1 : funDep              { [$1] }
         | funDep ',' funDeps1 { $1:$3 }

funcDef : func funcGenerics ident '(' types ')' typeMaybe
            { Function (tokPos $1) (fst $2) (snd $2) (Sym [tokStr $3]) $ foldType (Type.Func : maybe Tuple id $7 : $5) }
        | func funcGenerics ident '::' callType
            { Function (tokPos $1) (fst $2) (snd $2) (Sym [tokStr $3]) $5 }
        | func funcGenerics ident '::' type_
            { Function (tokPos $1) (fst $2) (snd $2) (Sym [tokStr $3]) $5 }


instDef : inst generics callType '(' args ')' instRetty scope
            { Instance (tokPos $1) $2 $3 $5 $7 $8 }


instRetty : {-empty-}  { False }
             | '->' '&'   { True  }



arg : ident      { Param    (tokPos $1) (Sym [tokStr $1]) Tuple }
    | ident '&'  { RefParam (tokPos $1) (Sym [tokStr $1]) Tuple }

args : {-empty-}       { [] }
     | args1           { $1 }
args1 : arg            { [$1] }
      | arg ',' args1  { ($1 : $3) }


fnDef : fn generics ident '(' paramsA ')' retty scope 
            { FuncInst (tokPos $1) $2 (Sym [tokStr $3]) $5 $7 $8 }


retty : {-empty-}     { Retty Tuple }
      | type_         { Retty $1 }
      | '&' type_     { RefRetty $2 }
      | '[' ']' type_ { Retty (foldl Apply Type.Slice [$3]) }

line : let pattern '=' expr               { Let (tokPos $1) $2 (Just $4) Nothing }  
     | let pattern                        { Let (tokPos $1) $2 Nothing Nothing }
     | expr                               { ExprStmt $1 }
     | expr '=' expr                      { ExprStmt (Call (tokPos $2) (TypeDef $ Sym ["store"]) [Reference (tokPos $2) $1, $3]) }
     | type generics Symbol type_         { Typedef (fst $3) $2 (snd $3) $4 }
     | return mexpr                       { Return (tokPos $1) $2 }
     | embed_c                            { AST.EmbedC (tokPos $1) [] (tokStr $1) }
     | enumMacro                          { $1 }
     | tupleMacro                         { $1 }
     | funcDef                            { $1 }
     | derivesDef                         { $1 }

block : if_                               { $1 }
      | while condition scope             { While (tokPos $1) $2 $3 }
      | for expr scope                    { For (tokPos $1) $2 Nothing $3 }
      | for expr '->' pattern scope       { For (tokPos $1) $2 (Just $4) $5 }
      | switch expr 'I' cases1 'D'        { Switch (tokPos $1) $2 $4 }
      | let pattern '='  expr in scope    { Let (tokPos $1) $2 (Just $4) (Just $6) }
      | let pattern in scope              { Let (tokPos $1) $2 Nothing (Just $4) }
      | fnDef                             { $1 }
      | instDef                           { $1 }

if_   : if condition scope else_          { If (tokPos $1) $2 $3 $4 }
      | if condition 'N' else_            { If (tokPos $1) $2 (Block []) $4 }
else_ : else scope                        { Just $2 }
      | else if_                          { Just $2 }
      | {-empty-}                         { Nothing }

scope  : 'I' stmts 'D'                    { Block $2 }
       | ';' line 'N'                     { Block [$2] }
       | ';' 'N'                          { Block [] }

cases1 : case                             { [$1] }
       | case cases1                      { $1 : $2 }
case : pattern scope                      { ($1, $2) }


--------------------------------------------------------------------------------------------------
-- Misc ------------------------------------------------------------------------------------------

Idents : {-empty-}                       { [] }
       | Idents1                         { $1 }

Idents1 : Ident                          { [tokStr $1] }
        | Ident ',' Idents1              { (tokStr $1):($3) } 

idents1 : ident                          { [tokStr $1] }
        | ident ',' idents1              { (tokStr $1):$3 }


symbol : ident                           { (tokPos $1, Sym [tokStr $1]) }
       | ident '::' ident                { (tokPos $3, Sym [tokStr $1, tokStr $3]) }
       | Ident '::' ident                { (tokPos $1, Sym [tokStr $1, tokStr $3]) }

symbols1 : symbol                        { [$1] }
         | symbol ',' symbols1           { $1:$3}

Symbol : Ident                           { (tokPos $1, Sym [tokStr $1]) }
       | ident '::' Ident                { (tokPos $3, Sym [tokStr $1, tokStr $3]) }


param   : ident type_                    { Param (tokPos $1)    (Sym [tokStr $1]) $2 }
        | ident '&' type_                { RefParam (tokPos $1) (Sym [tokStr $1]) $3 }
        | ident '[' ']' type_            { Param (tokPos $2)    (Sym [tokStr $1]) (foldl Apply Type.Slice [$4]) }
        | ident '&' '[' ']' type_        { RefParam (tokPos $2) (Sym [tokStr $1]) (foldl Apply Type.Slice [$5]) }
params  : {- empty -}                    { [] }
        | params1                        { $1 }
params1 : param                          { [$1] }
        | param ',' params1              { $1 : $3 }

paramsN : param ',' 'N'                  { [$1] } 
        | param ',' 'N' paramsN          { $1 : $4 }
paramsA : params                         { $1 }
        | 'I' paramsN 'D'                { $2 }
paramsA1 : params1                       { $1 }
         | 'I' paramsN 'D'               { $2 }


generics : {-empty-}                      { [] }
         | '{' Idents1 '}'                { map (\s -> Symbol.Sym [s]) $2 }

---------------------------------------------------------------------------------------------------
-- Patterns ---------------------------------------------------------------------------------------

patterns  : {- empty -}                  { [] }
          | patterns1                    { $1 }
patterns1 : pattern                      { [$1] }
          | pattern ',' patterns1        { $1 : $3 }

pattern  : '_'                           { PatIgnore (tokPos $1) }
         | literal                       { PatLiteral $1 }
         | '-' int_c                     { PatLiteral (AST.Int (tokPos $1) $ 0 - (read $ tokStr $2)) }
         | ident                         { PatIdent (tokPos $1) (Sym [tokStr $1]) }
         | '(' patterns ')'              { PatTuple (tokPos $1) $2 }
         | pattern '|' expr              { PatGuarded (tokPos $2) $1 (AExpr Type.Bool $3) }
         | pattern '|' expr '->' pattern { PatGuarded (tokPos $2) $1 (Match (tokPos $4) $3 $5) }
         | pattern ':' type_             { PatAnnotated $1 $3 }
         | symbol '(' patterns ')'       { PatField (tokPos $2) (snd $1) $3 }
         | '[' patterns ']'              { PatSlice (tokPos $1) $2 } 
 
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
       | expr ':' type_                          { AExpr $3 $1 }
       | symbol                                  { AST.Ident (fst $1) (snd $1) }
       | '[' exprsA ']'                          { Call (tokPos $1) (TypeDef $ Sym ["makeSlice"]) [AST.Array (tokPos $1) $2] }
       | expr '.' int_c                          { Call (tokPos $2) (foldType $ [TypeDef (Sym ["builtin", "field"]), Size (read $ tokStr $3), Type 0, Type 0]) [AST.Reference (tokPos $2) $1] }
       | '&' expr                                { AST.Reference (tokPos $1) $2 }
       | expr '[' expr ']'                       { Call (tokPos $2) (TypeDef $ Sym ["container", "at"]) [AST.Reference (tokPos $2) $1, $3] }
       | expr '.' callType                       { Call (tokPos $2) $3 (AST.Reference (tokPos $2) $1 : []) }
       | expr '.' callType  '(' exprsA ')'       { Call (tokPos $4) $3 (AST.Reference (tokPos $2) $1 : $5) }
       | callType '(' exprsA ')'                 { Call (tokPos $2) $1 $3 }
       | Symbol '(' exprsA ')'                   { AExpr (TypeDef $ snd $1) $ case $3 of [x] -> x; xs -> Call (tokPos $2) (TypeDef $ Sym ["tuple", "make" ++ show (length xs) ]) $3 }
       | '(' exprsA ')'                          { case $2 of [x] -> x; xs -> Call (tokPos $1) (TypeDef $ Sym ["tuple", "make" ++ show (length xs) ]) $2 }
       | expr '+' expr                           { Call (tokPos $2) (TypeDef $ Sym ["add"]) [$1, $3] }
       | expr '-' expr                           { Call (tokPos $2) (TypeDef $ Sym ["subtract"]) [$1, $3] } 
       | expr '*' expr                           { Call (tokPos $2) (TypeDef $ Sym ["multiply"]) [$1, $3] } 
       | expr '/' expr                           { Call (tokPos $2) (TypeDef $ Sym ["divide"]) [$1, $3] }
       | expr '%' expr                           { Call (tokPos $2) (TypeDef $ Sym ["modulo"]) [$1, $3] } 
       | expr '<' expr                           { Call (tokPos $2) (TypeDef $ Sym ["lessThan"]) [$1, $3] } 
       | expr '>' expr                           { Call (tokPos $2) (TypeDef $ Sym ["greaterThan"]) [$1, $3] } 
       | expr '==' expr                          { Call (tokPos $2) (TypeDef $ Sym ["equal"]) [$1, $3] } 
       | expr '!=' expr                          { Call (tokPos $2) (TypeDef $ Sym ["notEqualTo"]) [$1, $3] } 
       | expr '&&' expr                          { Call (tokPos $2) (TypeDef $ Sym ["boolean", "and"]) [$1, $3] } 
       | expr '||' expr                          { Call (tokPos $2) (TypeDef $ Sym ["boolean", "or"]) [$1, $3] } 
       | expr '<=' expr                          { Call (tokPos $2) (TypeDef $ Sym ["compare", "lessThanEqual"]) [$1, $3] } 
       | expr '>=' expr                          { Call (tokPos $2) (TypeDef $ Sym ["compare", "greaterThanEqual"]) [$1, $3] } 
--       | expr '<=' expr                          { Call (tokPos $2) (Sym ["lessEqual"]) [$1, $3] } 
--       | expr '>=' expr                          { Call (tokPos $2) (Sym ["greaterEqual"]) [$1, $3] } 
--       | expr '!=' expr                          { Call (tokPos $2) (Sym ["Boolean", "not"]) [Call (tokPos $2) (Sym ["Compare", "equal"]) [$1, $3]] } 
--       | expr '||' expr                          { Call (tokPos $2) (Sym ["Boolean", "or"]) [$1, $3] } 
       | '!' expr                                { Call (tokPos $1) (TypeDef $ Sym ["boolean", "not"]) [$2] }
       | '-' expr                                { Call (tokPos $1) (TypeDef $ Sym ["arithmetic", "negate"]) [$2] }


literal : int_c                                  { Call (tokPos $1) (TypeDef $ Sym ["convert", "convert"]) [AST.Int (tokPos $1) (read $ tokStr $1)] }
        | float_c                                { Call (tokPos $1) (TypeDef $ Sym ["convert", "convert"]) [AST.Float (tokPos $1) (read $ tokStr $1)] }
        | char_c                                 { AST.Char (tokPos $1) (read $ tokStr $1) }
        | true                                   { AST.Bool (tokPos $1) True }
        | false                                  { AST.Bool (tokPos $1) False }
        | string_c                               { AST.String (tokPos $1) (processString $ tokStr $1) }

---------------------------------------------------------------------------------------------------
-- Types ------------------------------------------------------------------------------------------

typeMaybe : {-empty-} { Nothing }
          | type_     { Just $1 }


callTypes1 : callType                { [$1] }
           | callType ',' callTypes1 { $1 : $3 }

callType : symbol                  { TypeDef (snd $1) }
         | symbol typeArgs         { foldType (TypeDef (snd $1) : $2) }


type__ : type_ { $1 }
       | int_c { Size (read $ tokStr $1) }

types  : {-empty-}                         { [] }
       | types1                            { $1 }
types1 : type__                             { [$1] }
       | type__ ',' types1                  { $1 : $3 }

types1N : type__ 'N'                        { [$1] }
        | type__ ',' 'N'                    { [$1] }
        | type__ ',' 'N' types1N            { $1 : $4 }
types2N : type__ ',' 'N' types1N            { $1 : $4 }


typeArgs : '{' types  '}'          { $2 }
         | '{' 'I' types1N 'D' '}' { $3 }
    

type_     : ordinal_t                      { $1 }
          | '(' types ')'                  { foldl Apply Tuple $2 }
          | Symbol                         { TypeDef (snd $1) }
          | Symbol typeArgs                { foldl Apply (TypeDef $ snd $1) $2 }
          | type_ '.' type_                  { Apply $3 $1 }

ordinal_t   : Bool                         { Type.Bool }
            | U8                           { U8 }
            | I8                           { I8 }
            | I16                          { I16 }
            | I32                          { I32 }
            | I64                          { I64 }
            | F32                          { F32 }
            | F64                          { F64 }
            | Char                         { Type.Char }

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


processString :: String -> String
processString string = case string of
    ('\\' : 'n' : xs) -> '\n' : processString xs
    ('\\' : 't' : xs) -> '\t' : processString xs
    ('\\' : '0' : xs) -> '\0' : processString xs
    ('\\' : '\\': xs) -> '\\' : processString xs
    (x : xs) -> x : processString xs
    [] -> []

}
