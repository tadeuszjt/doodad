{
{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Lexer
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
%right     '..'
%right     '!'
%left      '<-'
%left      ':'
%nonassoc  '->'
%nonassoc  '.'
%nonassoc  '::'
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
    '::'       { Token _ ReservedOp "::" }
    '<-'       { Token _ ReservedOp "<-" }
    '->'       { Token _ ReservedOp "->" }
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
    copy       { Token _ Reserved "copy" }
    for        { Token _ Reserved "for" }
    print      { Token _ Reserved "print" }
    unsafe_ptr { Token _ Reserved "unsafe_ptr" }
    zero       { Token _ Reserved "zero" }
    null       { Token _ Reserved "null" }
    data       { Token _ Reserved "data" }
    push       { Token _ Reserved "push" }
    pop        { Token _ Reserved "pop" }
    len        { Token _ Reserved "len" }
    clear      { Token _ Reserved "clear" }
    delete     { Token _ Reserved "delete" }

    import     { Token _ Import _ }
    import_c   { Token _ ImportC _ }
    import_c_macro { Token _ ImportCMacro _ }

    i16        { Token _ Reserved "i16" }
    i32        { Token _ Reserved "i32" }
    i64        { Token _ Reserved "i64" }
    f32        { Token _ Reserved "f32" }
    f64        { Token _ Reserved "f64" }
    bool       { Token _ Reserved "bool" }
    char       { Token _ Reserved "char" }
    string     { Token _ Reserved "string" }
    sparse     { Token _ Reserved "sparse" }

    intlit     { Token _ Int _ }
    floatlit   { Token _ Float _ }
    charlit    { Token _ Char _ }
    strlit     { Token _ String _ }
    ident      { Token _ Ident _ }

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

prog  : prog_                                 { S.AST Nothing [] $1 }
      | module ident 'N' imports prog_        { S.AST (Just (tokStr $2)) $4 $5 }
prog_ : {-empty-}                             { [] }
      | stmtS                                 { [$1] }
      | stmtS 'N' prog_                       { $1 : $3 }
      | stmtB prog_                           { $1 : $2 }

imports : {- empty -}                         { [] }
        | import 'N' imports                  { S.Import  (dropWhile isSpace $ dropWhile (`elem` "import_c") $ tokStr $1) : $3 }
        | import_c 'N' imports                { S.ImportC (dropWhile isSpace $ dropWhile (`elem` "import_c") $ tokStr $1) : $3 }
        | import_c_macro ident type_ 'N' imports { S.ImportCMacro (tokStr $2) $3 : $5 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------

symbol : ident                                { (tokPos $1, Sym (tokStr $1)) }
       | ident '::' ident                     { (tokPos $3, SymQualified (tokStr $1) (tokStr $3)) }

mfnrec : {-empty-}                            { [] }
       | '{' params1 '}'                      { $2 }

stmtS : let pattern '=' expr                         { S.Assign (tokPos $1) $2 $4 }  
      | index '=' expr                               { S.Set (tokPos $2) $1 $3 }
      | index                                        { S.ExprStmt $1 }
      | type symbol annoType                         { S.Typedef (fst $2) (snd $2) $3 }
      | print '(' exprs ')'                          { S.Print (tokPos $1) $3 }
      | return mexpr                                 { S.Return (tokPos $1) $2 }
      | data symbol type_                            { S.Data (tokPos $1) (snd $2) $3 }
stmtB : If                                           { $1 }
      | fn mfnrec ident '(' params ')' type_ block  { S.FuncDef (tokPos $1) $2 (tokStr $3) $5 $7 $8 }
      | fn mfnrec ident '(' params ')' block        { S.FuncDef (tokPos $1) $2 (tokStr $3) $5 T.Void $7 }
      | while condition block                        { S.While (tokPos $1) $2 $3 }
      | for expr block                        { S.For (tokPos $1) $2 Nothing $3 }
      | for expr '->' pattern block           { S.For (tokPos $1) $2 (Just $4) $5 }
      | Switch                                       { $1 }

pattern  : '_'                                { S.PatIgnore (tokPos $1) }
         | literal                            { S.PatLiteral $1 }
         | ident                              { S.PatIdent (tokPos $1) (Sym $ tokStr $1) }
         | null                               { S.PatNull (tokPos $1) }
         | '(' patterns ')'                   { S.PatTuple (tokPos $1) $2 }
         | '[' patterns ']'                   { S.PatArray (tokPos $1) $2 }
         | pattern '|' expr                   { S.PatGuarded (tokPos $2) $1 $3 }
         | symbol '(' patterns ')'            { S.PatField (tokPos $2) (snd $1) $3 }
         | typeOrdinal '(' pattern ')'        { S.PatTypeField (tokPos $2) $1 $3 }
         | pattern ':' type_                  { S.PatAnnotated $1 $3 }

patterns  : {- empty -}                       { [] }
          | patterns1                         { $1 }
patterns1 : pattern                           { [$1] }
          | pattern ',' patterns1             { $1 : $3 }

block  : 'I' prog_ 'D'                        { S.Block $2 }
       | ';' stmtS 'N'                        { $2 }
       | ';' 'N'                              { S.Block [] }

condition : expr                              { $1 }
          | expr '->' pattern                 { S.Match (tokPos $2) $1 $3 }

param   : ident type_                         { S.Param (tokPos $1) (Sym $ tokStr $1) $2 }
params  : {- empty -}                         { [] }
        | params1                             { $1 }
params1 : param                               { [$1] }
        | param ',' params1                   { $1 : $3 }

If    : if condition block else_              { S.If (tokPos $1) $2 $3 $4 }
      | if condition 'N' else_                { S.If (tokPos $1) $2 (S.Block []) $4 }
else_ : else block                            { Just $2 }
      | else If                               { Just $2 }
      | {-empty-}                             { Nothing }


Switch : switch expr 'I' cases1 'D'           { S.Switch (tokPos $1) $2 $4 }
       | switch expr                          { S.Switch (tokPos $1) $2 [] }
cases1 : case                                 { [$1] }
      | case cases1                           { $1 : $2 }
case : pattern block                          { ($1, $2) }

---------------------------------------------------------------------------------------------------
-- Expressions ------------------------------------------------------------------------------------

exprs  : {- empty -}                          { [] }
       | exprs1                               { $1 }
exprs1 : expr                                 { [$1] }
       | expr ',' exprs1                      { $1 : $3 }
exprsN : expr 'N'                             { [$1] } 
       | expr ',' 'N' exprsN                  { $1 : $4 }

mexpr : {-empty-}                             { Nothing }
      | expr                                  { Just $1 }

call : symbol '(' exprs ')'                   { (S.Call (tokPos $2) (snd $1) $3, tokPos $2) }

index  : symbol                               { S.Ident (fst $1) (snd $1) }
       | index '[' expr ']'                   { S.Subscript (tokPos $2) $1 $3 }
       | index '.' ident                      { S.Field (tokPos $2) $1 (tokStr $3) }
       | index '.' ident '(' exprs ')'        { S.CallMember (tokPos $2) $1 (Sym $ tokStr $3) $5 }
       | index '.' push '(' exprs ')'         { S.Push (tokPos $2) $1 $5 }
       | index '.' pop '(' exprs ')'          { S.Pop (tokPos $2) $1 $5 }
       | index '.' clear '(' ')'              { S.Clear (tokPos $2) $1 }
       | index '.' delete '(' expr ')'        { S.Delete (tokPos $2) $1 $5 }
       | call                                 { (fst $1) }

expr   : literal                              { $1 }
       | infix                                { $1 }
       | prefix                               { $1 }
       | call                                 { (fst $1) }
       | symbol                               { S.Ident (fst $1) (snd $1) }
       | '[' exprs1 ']'                       { S.Array (tokPos $1) $2 }
       | '[' ']'                              { S.Array (tokPos $1) [] }
       | '[' 'I' exprsN 'D' ']'               { S.Array (tokPos $1) $3 }
       | '(' expr ')'                         { $2 }
       | '(' expr ',' exprs1 ')'              { S.Tuple (tokPos $1) ($2:$4) }
       | unsafe_ptr '(' expr ')'              { S.UnsafePtr (tokPos $1) $3 }
       | typeOrdinal '(' exprs ')'            { S.Conv (tokPos $2) $1 $3 }
       | null                                 { S.Null (tokPos $1) }
       | ':' typeAggregate '(' exprs ')'      { S.Conv (tokPos $3) $2 $4 }
       | expr '.' intlit                      { S.TupleIndex (tokPos $2) $1 (read $ tokStr $3) }
       | expr '.' ident                       { S.Field (tokPos $2) $1 (tokStr $3) }
       | expr '[' expr ']'                    { S.Subscript (tokPos $2) $1 $3 }
       | expr ':' type_                       { S.AExpr $3 $1 }
       | '{' expr '}'                         { S.ADT (tokPos $1) $2 }
       | expr '.' ident '(' exprs ')'         { S.CallMember (tokPos $4) $1 (Sym $ tokStr $3) $5 }
       | expr '.' push '(' exprs ')'          { S.Push (tokPos $4) $1 $5 }
       | expr '.' pop '(' exprs ')'           { S.Pop (tokPos $4) $1 $5 }
       | expr '.' len '(' ')'                 { S.Len (tokPos $4) $1 }
       | expr '.' clear '(' ')'               { S.Clear (tokPos $4) $1 }
       | len '(' expr ')'                     { S.Len (tokPos $1) $3 }
       | expr '[' mexpr '..' mexpr ']'        { S.Range (tokPos $2) (Just $1) $3 $5 }
       | '[' mexpr '..' mexpr ']'             { S.Range (tokPos $1) Nothing $2 $4 }

tableRows : exprs1                             { [$1] }
          | exprs1 ';' tableRows               { $1 : $3 }


literal : intlit                              { S.Int (tokPos $1) (read $ tokStr $1) }
        | floatlit                            { S.Float (tokPos $1) (read $ tokStr $1) }
        | charlit                             { S.Char (tokPos $1) (read $ tokStr $1) }
        | strlit                              { S.String (tokPos $1) (tokStr $1) }
        | true                                { S.Bool (tokPos $1) True }
        | false                               { S.Bool (tokPos $1) False }

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


prefix : '-' expr                             { S.Prefix (tokPos $1) S.Minus $2 }
       | '+' expr                             { S.Prefix (tokPos $1) S.Plus $2 }
       | '!' expr                             { S.Prefix (tokPos $1) S.Not $2 }


---------------------------------------------------------------------------------------------------
-- Types ------------------------------------------------------------------------------------------
mtype  : {-empty-}                            { Nothing }
       | type_                                { Just $1 }


type_         : symbol                        { T.Typedef (snd $1) }
              | typeOrdinal                   { $1 }
              | typeAggregate                 { $1 }

typeOrdinal   : bool                          { T.Bool }
              | i16                           { T.I16 }
              | i32                           { T.I32 }
              | i64                           { T.I64 }
              | f32                           { T.F32 }
              | f64                           { T.F64 }
              | char                          { T.Char }
              | string                        { T.String }

typeAggregate : tableType                     { $1 }
              | arrayType                     { $1 }
              | tupType                       { $1 }
              | adtType                       { $1 }
              | sparseType                    { $1 }
              | rangeType                     { $1 }
              | fn '(' argTypes ')' type_     { T.Func $3 $5 }


adtType : '{' adtFields '}'                   { T.ADT $2 }
arrayType : '[' intlit type_ ']'              { T.Array (read $ tokStr $2) $3 }
tableType : '[' rowTypes1 ']'                 { T.Table $2 }
tupType : '(' tupFields ')'                   { T.Tuple $2 }
sparseType : sparse '[' rowTypes1 ']'         { T.Sparse $3 }
rangeType : '[' '..' ']' type_                { T.Range $4 }


annoType : typeOrdinal                        { S.AnnoType $1 }
         | symbol                             { S.AnnoType (T.Typedef $ snd $1) }
         | tupType                            { S.AnnoType $1 }
         | arrayType                          { S.AnnoType $1 }
         | tableType                          { S.AnnoType $1 }
         | sparseType                         { S.AnnoType $1 }
         | annoTupType                        { $1 }
         | annoADTType                        { $1 }


adtFields : {-empty-}                     { [] }
          | adtFields1                    { $1 }
adtFields1 : adtField                     { [$1] }
           | adtField '|' adtFields1      { $1 : $3 }
adtField : type_                          { T.FieldType $1 }
         | null                           { T.FieldNull }


tupFields : {-empty -}                    { [] }
          | tupFields1                    { $1 }
tupFields1 : type_                        { [$1] }
           | type_ ',' tupFields1         { $1 : $3 }


argTypes  : {-empty -}                        { [] }
          | argTypes1                         { $1 }
argTypes1 : type_                             { [$1] }
          | type_ ',' argTypes1               { $1:$3 }


annoTupType : '(' annoTupFields ')'           { S.AnnoTuple $2 }
annoTupField : ident type_                    { (tokStr $1, $2) }
annoTupFields : annoTupField                  { [$1] }
              | annoTupField ',' annoTupFields { $1 : $3 }

annoADTType : '{' annoADTFields '}'           { S.AnnoADT $2 }
            | '{' 'I' annoADTFields 'D' '}'   { S.AnnoADT $3 }
            | '{' '}'                         { S.AnnoADT [] }
annoADTField : ident '(' argTypes ')'         { S.ADTFieldMember (Sym (tokStr $1)) $3 }
             | type_                          { S.ADTFieldType $1 }
             | null                           { S.ADTFieldNull }
annoADTFields : annoADTField                  { [$1] }
annoADTFields : annoADTField 'N'              { [$1] }
              | annoADTField '|' annoADTFields { $1 : $3 }
              | annoADTField '|' 'N' annoADTFields { $1 : $4 }

rowTypes1     : type_                         { [$1] }
              | type_ ';' rowTypes1           { $1 : $3 }
{
parse :: MonadError Error m => FilePath -> String -> m S.AST
parse filePath source = do
    case alexScanner filePath source of
        Left  errStr -> throwError (ErrorStr errStr)
        Right tokens -> case (parseTokens tokens) 0 of
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
