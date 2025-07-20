{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where
import Token
import Error
import Control.Monad.Except hiding (void, fail)
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map
import Type
import AST
import Symbol
import AstBuilder
import InstBuilder
}

%name      parseTokens 
%tokentype { Token }
%monad { Parser } { >>= } { return }

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
%nonassoc  '@'


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
    with       { Token _ Token.Reserved "with" }
    func       { Token _ Token.Reserved "func" }
    inst       { Token _ Token.Reserved "inst" }
    derives    { Token _ Token.Reserved "derives" }
    module     { Token _ Token.Module _ }
    import     { TokenImport _ _ _ _ _ }
    include    { Token _ Token.CInclude _ }
    link       { Token _ Token.CLink _ }

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

prog  : topStmts             {% liftAstBuilderState $ modify $ \s -> s { abModuleName = "main" } }
      | header topStmts      {% liftAstBuilderState $ modify $ \s -> s { abModuleName = (tokStr $1) } }
header : module 'N' imports  { $1 }
imports: {- empty -}         { () }
       | import 'N' imports {% addImport $ AST.Import (tokImpExp $1) (tokImpQual $1) (tokImpPath $1) (tokImpAs $1) }
       | include 'N' imports {% addImport $ AST.CInclude (tokStr $1) }
       | link 'N' imports    {% addImport $ AST.CLink (tokStr $1) }


topStmts : {-empty-}                  { () }
         | topStmtLine 'N' topStmts   { () }
         | topStmtBlock topStmts      { () }


topStmtLine
    : type generics Symbol type_                {% addTopStmt $ TopStmt $ Typedef $2 (snd $3) $4 }
    | enumMacro                                 {% addTopStmt (TopStmt $1) }
    | derives generics type_ '(' callTypes1 ')' {% addTopStmt $ TopStmt (Derives $2 $3 $5) }
    | func funcGenerics ident '(' types ')' typeMaybe {%
        addTopStmt $ TopStmt $ Function (fst $2) (snd $2) (Sym [tokStr $3]) $
            foldType (Type.Func : maybe Tuple id $7 : $5)
    }
    | func funcGenerics ident '::' callType {%
        addTopStmt $ TopStmt $ Function (fst $2) (snd $2) (Sym [tokStr $3]) $5
    }
    | func funcGenerics ident '::' type_ {%
        addTopStmt $ TopStmt $ Function (fst $2) (snd $2) (Sym [tokStr $3]) $5
    }
    | tuple generics Ident '{' tupleFields '}' {% do
        let pos = tokPos $1
        let generics = $2
        let fields = $5
        let symbol = (Sym [tokStr $3])
        void $ addTopStmt $ TopStmt $ Typedef generics symbol (foldl Apply Tuple $ map snd fields)
        forM_ (zip fields [0..]) $ \((fieldSymbol, fieldType), i) -> do
            -- write field feature
            let a = Sym ["A"]
            let b = Sym ["B"]
            addTopStmt $ TopStmt $ Function [a, b] [(a, b)] fieldSymbol $ foldType [Type.Func, TypeDef b, TypeDef a]

            -- write instance
            let typ = foldType (TypeDef symbol : map TypeDef generics)
            let acq = foldType [TypeDef fieldSymbol, typ, fieldType]
            identExpr <- newExpr pos $ AST.Ident (Sym ["x"])
            appendStmt . Return . Just =<< field pos fieldType i identExpr
            inst0 <- takeInst 0

            addTopStmt $ TopInst pos generics acq [Param pos (Sym ["x"]) (Apply Type.Ref Tuple)] True inst0

            let acq2 = foldType [TypeDef fieldSymbol, Apply Table typ, Apply Slice fieldType]
            identExpr2 <- newExpr pos $ AST.Ident (Sym ["x"])
            appendStmt . Return . Just =<< field pos (Apply Slice fieldType) i identExpr2
            inst1 <- takeInst 0
            addTopStmt $ TopInst pos generics acq2 [Param pos (Sym ["x"]) (Apply Ref Tuple)] False inst1
    }

topStmtBlock
    : inst generics callType '(' args ')' instRetty scope {% do
        addTopStmt . TopInst (tokPos $1) $2 $3 $5 $7 =<< takeInst $8
    }
    | fn generics ident '(' params ')' retty scope {% do
        argTypes <- forM $5 $ \param -> case param of
            Param pos symbol (Apply Type.Ref t) -> return t
            Param pos symbol t                  -> return t
     
        isRef <- case $7 of
            RefRetty _ -> return True
            Retty _    -> return False

        addTopStmt $ TopStmt $ Function $2 [] (Sym[tokStr $3]) $ foldType (Type.Func : typeof $7 : argTypes)

        let callType = foldl Apply (TypeDef $ Sym [tokStr $3]) $ map TypeDef $2

        addTopStmt . TopInst (tokPos $1) $2 callType $5 isRef =<< takeInst $8
    }

---------------------------------------------------------------------------------------------------
-- Macros -----------------------------------------------------------------------------------------

enumMacro : enum generics Ident '{' enumCases '}' { AST.Enum $2 (Sym [tokStr $3]) $5 }

enumCases : {-empty-}                         { [] }
          | enumCase                          { [$1] }
          | enumCase ',' enumCases            { $1 : $3 }
enumCase : ident               { (Sym [tokStr $1], []) }
         | ident '(' types ')' { (Sym [tokStr $1], $3) }


tupleField : symbol type_    { (snd $1, $2) }
tupleFields : {-empty-}                  { [] }
            | tupleField                 { [$1] }
            | tupleField ',' tupleFields { $1 : $3 }


---------------------------------------------------------------------------------------------------
-- Statements -------------------------------------------------------------------------------------


funcGenerics : '{' Idents  funDeps '}'  { (map (\s -> Symbol.Sym [s]) $2, $3) }
             | {-empty-}                { ([], []) }

funDeps : {-empty-}    { [] }
        | '|' funDeps1 { $2 }

funDep : Ident '->' Ident { (Symbol.Sym [tokStr $1], Symbol.Sym [tokStr $3]) }
funDeps1 : funDep              { [$1] }
         | funDep ',' funDeps1 { $1:$3 }

instRetty : {-empty-}  { False }
          | '->' '&'   { True  }


arg : ident      { Param    (tokPos $1) (Sym [tokStr $1]) Tuple }
    -- | ident '&'  { RefParam (tokPos $1) (Sym [tokStr $1]) Tuple }
    | ident '&'  { Param (tokPos $1) (Sym [tokStr $1]) (Apply Type.Ref Tuple) }

args : {-empty-}       { [] }
     | args1           { $1 }
args1 : arg            { [$1] }
      | arg ',' args1  { ($1 : $3) }


retty : {-empty-}     { Retty Tuple }
      | type_         { Retty $1 }
      | '&' type_     { RefRetty $2 }
      | '[' ']' type_ { Retty (foldl Apply Type.Slice [$3]) }

line : let pattern '=' expr               {% newStmt (tokPos $1) $ Let $2 (Just $4) Nothing }  
     | let pattern                        {% newStmt (tokPos $1) $ Let $2 Nothing Nothing }
     | expr                               {% newStmt (TextPos "" 0 0) $ ExprStmt $1 }
     | expr '=' expr                      {% do
    refExpr <- newExpr (tokPos $2) $ Reference $1
    calExpr <- newExpr (tokPos $2) $ Call (TypeDef $ Sym ["store"]) [refExpr, $3]
    newStmt (tokPos $2) $ ExprStmt calExpr
}
     | return mexpr                       {% newStmt (tokPos $1) $ Return $2 }
     | embed_c                            {% newStmt (tokPos $1) $ AST.EmbedC [] (tokStr $1) }

block : if_                               { $1 }
      | while condition scope             {% newStmt (tokPos $1) $ While $2 $3 }
      | for expr scope                    {% newStmt (tokPos $1) $ For $2 Nothing $3 }
      | for expr '->' pattern scope       {% newStmt (tokPos $1) $ For $2 (Just $4) $5 }
      | switch expr 'I' cases1 'D'        {% newStmt (tokPos $1) $ Switch $2 $4 }
      | let pattern '='  expr in scope    {% newStmt (tokPos $1) $ Let $2 (Just $4) (Just $6) }
      | let pattern in scope              {% newStmt (tokPos $1) $ Let $2 Nothing (Just $4) }
      | with '(' exprs ')' scope          {% newStmt (tokPos $1) (With $3 $5) }

if_   : if condition scope else_          {% newStmt (tokPos $1) $ If $2 $3 $4 }
      | if condition 'N' else_            {% do
    blk <- newStmt (tokPos $1) (Block [])
    newStmt (tokPos $1) $ If $2 blk $4
}
else_ : else scope                        { Just $2 }
      | else if_                          {% do
    blkId <- newStmt (tokPos $1) (Block [$2])
    return $ Just blkId
}
      | {-empty-}                         { Nothing }

stmts : {-empty-}                         { [] }
      | line 'N' stmts                    { $1 : $3 }
      | block stmts                       { $1 : $2 }
scope  : 'I' stmts 'D'                    {% newStmt (tokPos $1) $ Block $2 }
       | ';' line 'N'                     {% newStmt (tokPos $1) $ Block [$2] }
       | ';' 'N'                          {% newStmt (tokPos $1) $ Block [] }

cases1 : case                             { [$1] }
       | case cases1                      { $1 : $2 }
case : pattern scope                      { ($1, $2) }


--------------------------------------------------------------------------------------------------
-- Misc ------------------------------------------------------------------------------------------

Idents : {-empty-}                       { [] }
       | Ident                           { [tokStr $1] }
       | Ident ',' Idents                { (tokStr $1) : $3 }

idents1 : ident                          { [tokStr $1] }
        | ident ',' idents1              { (tokStr $1):$3 }


symbol : ident                           { (tokPos $1, Sym [tokStr $1]) }
       | ident '::' ident                { (tokPos $3, Sym [tokStr $1, tokStr $3]) }
       | Ident '::' ident                { (tokPos $1, Sym [tokStr $1, tokStr $3]) }

Symbol : Ident                           { (tokPos $1, Sym [tokStr $1]) }
       | ident '::' Ident                { (tokPos $3, Sym [tokStr $1, tokStr $3]) }


param   : ident type_                    { Param (tokPos $1)    (Sym [tokStr $1]) $2 }
        | ident '&' type_                { Param (tokPos $1) (Sym [tokStr $1]) (Apply Type.Ref $3) }
params  : {- empty -}                    { [] }
        | param                          { [$1] }
        | param ',' params               { $1 : $3 }

generics : {-empty-}                      { [] }
         | '{' Idents  '}'                { map (\s -> Symbol.Sym [s]) $2 }

---------------------------------------------------------------------------------------------------
-- Patterns ---------------------------------------------------------------------------------------

patterns  : {- empty -}                  { [] }
          | patterns1                    { $1 }
patterns1 : pattern                      { [$1] }
          | pattern ',' patterns1        { $1 : $3 }

pattern  : '_'                           {% newPattern $ PatIgnore }
         | literal                       {% newPattern $ PatLiteral $1 }
         | '-' int_c                     {% do
    intExpr <- newExpr (tokPos $1) $ AST.Int $ 0 - (read $ tokStr $2)
    newPattern $ PatLiteral intExpr
}
         | ident                         {% newPattern $ PatIdent (Sym [tokStr $1]) }
         | '(' patterns ')'              {% newPattern $ PatTuple $2 }
         | pattern '|' expr              {% do
    expr <- newExpr (tokPos $2) $ AExpr Type.Bool $3
    newPattern $ PatGuarded $1 expr
}
         | pattern '|' expr '->' pattern {% do
    matchExpr <- newExpr (tokPos $2) $ Match $3 $5
    newPattern $ PatGuarded $1 matchExpr
}
         | pattern ':' type_             {% newPattern $ PatAnnotated $1 $3 }
         | '[' patterns ']'              {% newPattern $ PatSlice $2 } 
         | symbol '(' patterns ')'       {% do
    tupPat <- case $3 of
        [x] -> return x
        xs -> newPattern $ PatTuple xs
    newPattern $ PatField (snd $1) tupPat
}
 
---------------------------------------------------------------------------------------------------
-- Expressions ------------------------------------------------------------------------------------

exprs  : {- empty -}                             { [] }
       | expr                                    { [$1] }
       | expr ',' exprs                          { $1 : $3 }
mexpr  : {-empty-}                               { Nothing }
       | expr                                    { Just $1 }

condition : expr                                 { $1 }
          | expr '->' pattern                    {% newExpr (tokPos $2) $ Match $1 $3 }

expr   : literal                                 { $1 }
       | expr ':' type_                          {% newExpr (tokPos $2) $ AExpr $3 $1 }
       | symbol                                  {% newExpr (fst $1) $ AST.Ident (snd $1) }
       | '[' exprs ']'                           {% do
    arrExpr <- newExpr (tokPos $1) $ AST.Array $2
    newExpr (tokPos $1) $ Call (TypeDef $ Sym ["slice", "makeSlice"]) [arrExpr]
}
       | expr '.' int_c                          {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    newExpr (tokPos $2) $ Call (foldType $ [TypeDef (Sym ["builtin", "field"]), Size (read $ tokStr $3), Type 0, Type 0]) [refExpr]
}
       | '&' expr                                {% newExpr (tokPos $1) $ AST.Reference $2 }
       | expr '[' expr ']'                       {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    newExpr (tokPos $2) $ Call (TypeDef $ Sym ["container", "at"]) [refExpr, $3]
}
       | expr '[' '..' ']'                       {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    intExpr <- newExpr (tokPos $2) $ AST.Int 0
    calExpr <- newExpr (tokPos $2) $ Call (TypeDef $ Sym ["limits", "limitMax"]) []
    newExpr (tokPos $2) $ Call (TypeDef $ Sym ["slice", "slice"]) [ refExpr , intExpr , calExpr ]
}
       | expr '[' '..' expr ']'                  {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    intExpr <- newExpr (tokPos $2) $ AST.Int 0
    newExpr (tokPos $2) $ Call (TypeDef $ Sym ["slice", "slice"]) [ refExpr , intExpr , $4 ]
}
       | expr '[' expr '..' ']'                  {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    callExpr <- newExpr (tokPos $2) $ Call (TypeDef $ Sym ["limits", "limitMax"]) []
    newExpr (tokPos $2) $ Call (TypeDef $ Sym ["slice", "slice"]) [ refExpr , $3 , callExpr ]
}
       | expr '[' expr '..' expr ']'             {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    newExpr (tokPos $2) $ Call (TypeDef $ Sym ["slice", "slice"]) [refExpr, $3, $5]
}
       | expr '.' callType                       {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    newExpr (tokPos $2) $ Call $3 [refExpr]
}
       | expr '.' callType  '(' exprs ')'        {% do
    refExpr <- newExpr (tokPos $2) $ AST.Reference $1
    newExpr (tokPos $4) $ Call $3 (refExpr : $5)
}
       | callType '(' exprs ')'                  {% newExpr (tokPos $2) $ Call $1 $3 }
       | Symbol '(' exprs ')'                    {% do
    expr <- case $3 of
        [x] -> return x
        xs  -> newExpr (tokPos $2) $ Call (TypeDef $ Sym ["tuple", "make" ++ show (length xs) ]) $3
    newExpr (tokPos $2) $ AExpr (TypeDef $ snd $1) expr
}
       | '(' exprs ')'                           {% case $2 of [x] -> return x; xs -> newExpr (tokPos $1) $ Call (TypeDef $ Sym ["tuple", "make" ++ show (length xs) ]) $2 }
       | expr '+' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["add"]) [$1, $3] }
       | expr '-' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["subtract"]) [$1, $3] } 
       | expr '*' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["multiply"]) [$1, $3] } 
       | expr '/' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["divide"]) [$1, $3] }
       | expr '%' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["modulo"]) [$1, $3] } 
       | expr '<' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["lessThan"]) [$1, $3] } 
       | expr '>' expr                           {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["greaterThan"]) [$1, $3] } 
       | expr '==' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["equal"]) [$1, $3] } 
       | expr '!=' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["notEqualTo"]) [$1, $3] } 
       | expr '&&' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["boolean", "and"]) [$1, $3] } 
       | expr '||' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["boolean", "or"]) [$1, $3] } 
       | expr '<=' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["compare", "lessThanEqual"]) [$1, $3] } 
       | expr '>=' expr                          {% newExpr (tokPos $2) $ Call (TypeDef $ Sym ["compare", "greaterThanEqual"]) [$1, $3] } 
       | '!' expr                                {% newExpr (tokPos $1) $ Call (TypeDef $ Sym ["boolean", "not"]) [$2] }
       | '-' expr                                {% newExpr (tokPos $1) $ Call (TypeDef $ Sym ["arithmetic", "negate"]) [$2] }
       | '@' type_                               {% newExpr (tokPos $1) $ Call (foldType [TypeDef (Sym ["builtin", "builtinContext"]), $2]) [] }


literal : int_c                                  {% do
    intExpr <- newExpr (tokPos $1) $ AST.Int (read $ tokStr $1)
    newExpr (tokPos $1) $ Call (TypeDef $ Sym ["convert", "convert"]) [intExpr]
}
        | float_c                                {% do
    floatExpr <- newExpr (tokPos $1) $ AST.Float (read $ tokStr $1)
    newExpr (tokPos $1) $ Call (TypeDef $ Sym ["convert", "convert"]) [floatExpr]
}
        | char_c                                 {% newExpr (tokPos $1) $ AST.Char (read $ tokStr $1) }
        | true                                   {% newExpr (tokPos $1) $ AST.Bool True }
        | false                                  {% newExpr (tokPos $1) $ AST.Bool False }
        | string_c                               {% do
    strExpr <- newExpr (tokPos $1) $ AST.String (processString $ tokStr $1)
    newExpr (tokPos $1) $ Call (TypeDef $ Sym ["slice", "makeSlice"]) [strExpr]
}

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


typeArgs : '{' types  '}'          { $2 }
    

type_  : '(' types ')' {%
    return (foldl Apply Tuple $2)
}
       | Symbol                         { TypeDef (snd $1) }
       | Symbol typeArgs                { foldl Apply (TypeDef $ snd $1) $2 }
       | type_ '.' type_                { Apply $3 $1 }

{
newtype Parser a = Parser { unParser :: StateT InstBuilderState (StateT AstBuilderState (Except Error)) a }
	deriving (Functor, Applicative, Monad, MonadError Error)


instance MonadAstBuilder (Parser) where
    liftAstBuilderState (StateT s) = Parser $ lift $ state (runIdentity . s)

instance MonadInstBuilder Parser where
    liftInstBuilderState (StateT s) = Parser $ state (runIdentity . s)


takeInst :: StmtID -> Parser InstBuilderState
takeInst id = liftInstBuilderState $ do
    stmt <- (Map.! id) <$> gets statements
    state <- get
    put initInstBuilderState
    return $ state { statements = (Map.insert 0 stmt $ Map.delete id $ statements state) }


runParser :: Parser a -> Either Error AstBuilderState
runParser p = runExcept $ (flip execStateT) initAstBuilderState $ runStateT (unParser p) initInstBuilderState 


tokPos :: Token -> TextPos
tokPos tok = tokPosn tok


field :: TextPos -> Type -> Integer -> ExprID -> Parser ExprID
field pos fieldType i expr = do 
    callExpr <- newExpr pos $ Call typ [expr]
    newExpr pos $ AExpr fieldType callExpr
    where
        typ = foldType [TypeDef (Sym ["builtin", "builtinField"]), Size (fromIntegral i), Type 0, Type 0]
    

happyError :: [Token] -> Parser a
happyError []    = throwError $ ErrorPos (TextPos "" 0 0) "parse error"
happyError (x:_) = throwError $ ErrorPos (tokPosn x) "parse error"


processString :: String -> String
processString string = case string of
    ('\\' : 'n' : xs) -> '\n' : processString xs
    ('\\' : 't' : xs) -> '\t' : processString xs
    ('\\' : '0' : xs) -> '\0' : processString xs
    ('\\' : '\\': xs) -> '\\' : processString xs
    (x : xs) -> x : processString xs
    [] -> []

}
