%token Indent       
%token Dedent       
%token Newline       
%token '+'       
%token '-'       
%token '*'       
%token '/'       
%token '%'       
%token '<'       
%token '>'       
%token '='       
%token '!'       
%token '@'       
%token NotEq      
%token LtEq      
%token GtEq      
%token EqEq      
%token AndAnd      
%token OrOr      
%token DoubleColon      
%token RArrow      
%token DoubleDot      
%token fn        
%token type      
%token if        
%token else      
%token let       
%token while     
%token return    
%token switch    
%token true      
%token false     
%token module    
%token for       
%token null      
%token data      
%token import    
%token import_c  
%token i16       
%token i32       
%token i64       
%token f32       
%token f64       
%token bool      
%token char      
%token string    
%token sparse    
%token map       
%token int_c     
%token float_c   
%token char_c    
%token string_c  
%token ident     
%token '('       
%token ')'       
%token '{'       
%token '}'       
%token '['       
%token ']'       
%token '|'       
%token ','       
%token '.'       
%token ';'       
%token ':'       
%token '_'       

%left      OrOr
%left      AndAnd
%left      EqEq NotEq
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  LtEq GtEq '<' '>'
%left      ':'
%nonassoc  '!'
%nonassoc  '|'
%nonassoc  '(' ')' '[' ']' '{' '}'
%nonassoc  '.'
%nonassoc  RArrow


%%

prog  : stmts                               
      | header stmts                        
      ;
stmts :                            
      | line Newline stmts                      
      | block stmts                         
      ;

header : module ident Newline imports           ;
imports :                        
        | import Newline imports                
        | import_c Newline imports              
        ;



symbol : ident                              
       | ident DoubleColon ident                   
       ;

mfnrec :                           
       | '{' paramsA '}'                    
       ;


initialiser : '{' exprsA '}'                
            | string_c                      
            ;
minitialiser : initialiser                  
             |                     
             ;

line : let pattern '=' expr                         
     | index '=' expr                               
     | index                                        
     | type symbol anno_t                           
     | return mexpr                                 
     | data symbol type_ minitialiser               
     ;
block : if_                                         
      | fn mfnrec ident '(' paramsA ')' mtype scope 
      | while condition scope                       
      | for expr scope                              
      | for expr RArrow pattern scope                 
      | switch_                                     
      ;

scope  : Indent stmts Dedent                      
       | ';' line Newline                       
       | ';' Newline                            
       ;

condition : expr                            
          | expr RArrow pattern               
          ;


param   : ident type_                       ;
params  :                        
        | params1                           
        ;
params1 : param                             
        | param ',' params1                 
        ;
paramsN : param Newline                         
        | param Newline paramsN                 
        ;
paramsA : params                            
        | Indent paramsN Dedent                   
        ;


if_   : if condition scope else_            
      | if condition Newline else_              
      ;
else_ : else scope                          
      | else if_                            
      |                            
      ;


switch_ : switch expr Indent cases1 Dedent        
       | switch expr                        
       ;
cases1 : case                               
      | case cases1                         
      ;
case : pattern scope                        ;


patterns  :                      
          | patterns1                       
          ;
patterns1 : pattern                         
          | pattern ',' patterns1           
          ;
patternsSem : patterns                      
           | patterns ';' patternsSem       
           ;

pattern  : '_'                              
         | literal                          
         | '-' int_c                        
         | ident                            
         | null                             
         | '(' patterns ')'                 
         | '[' patternsSem ']'              
         | pattern '|' expr                 
         | pattern '|' expr RArrow pattern    
         | symbol '(' patterns ')'          
         | ordinal_t '(' pattern ')'        
         | pattern ':' type_                
         ;


exprs  :                         
       | exprs1                             
       ;
exprs1 : expr                               
       | expr ',' exprs1                    
       ;
exprsN : expr Newline                           
       | expr Newline exprsN                    
       ;
mexpr  :                           
       | expr                               
       ;
exprsA : exprs                              
       | Indent exprsN Dedent                     
       ;

call : symbol '(' exprsA ')'                   
     | '{' exprsA '}' '.' ident '(' exprsA ')' 
     ;

index  : symbol                             
       | index '[' expr ']'                 
       | index '.' ident                    
       | index '.' symbol '(' exprsA ')'    
       | call                               
       ;

expr   : literal                            
       | infix                              
       | prefix                             
       | call                               
       | symbol                             
       | '(' exprsA ')'                     
       | ordinal_t '(' exprsA ')'           
       | null                               
       | expr '.' ident                     
       | expr '[' expr ']'                  
       | expr ':' type_                     
       | expr '.' ident '(' exprsA ')'      
       | expr '[' mexpr DoubleDot mexpr ']'      
       | '[' mexpr DoubleDot mexpr ']'           
       | '[' exprsA ']'                     
       ;

literal : int_c                             
        | float_c                           
        | char_c                            
        | string_c                          
        | true                              
        | false                             
        ;

infix : expr '+' expr                       
      | expr '-' expr                       
      | expr '*' expr                       
      | expr '/' expr                       
      | expr '%' expr                       
      | expr '<' expr                       
      | expr '>' expr                       
      | expr LtEq expr                      
      | expr GtEq expr                      
      | expr EqEq expr                      
      | expr AndAnd expr                      
      | expr OrOr expr                      
      | expr NotEq expr                      
      ;

prefix : '-' expr                           
       | '+' expr                           
       | '!' expr                           
       ;

mtype  :                           
       | type_                              
       ;
types  :                           
       | types1                             
       ;
types1 : type_                              
       | type_ ',' types1                   
       ;
types1_ : type_                             
        | type_ ';' types1_                 
        ;
    
type_         : symbol                      
              | ordinal_t                   
              | aggregate_t                 
              ;

ordinal_t   : bool                          
            | i16                           
            | i32                           
            | i64                           
            | f32                           
            | f64                           
            | char                          
            | '@' type_                     
            ;

aggregate_t : table_t                       
              | array_t                     
              | tup_t                       
              | adt_t                       
              | sparse_t                    
              | map_t                       
              | range_t                     
              | fn '(' types ')' type_      
              ;


adt_t    : '{' adtFields '}'                ;
array_t  : '[' int_c type_ ']'              ;
table_t  : '[' types1_ ']'                  
         | string                           
         ;
tup_t    : '(' types ')';                   
sparse_t : sparse '[' types1_ ']';          
map_t    : map '[' type_ ']' type_ ;
range_t  : '[' DoubleDot ']' type_ ;


anno_t   : ordinal_t                        
         | symbol                           
         | '(' types1 ')'                   
         | '(' paramsA ')'                  
         | array_t                          
         | table_t                          
         | sparse_t                         
         | '{' ADTFieldsA '}'               
         ;


adtFields :                        
          | adtFields1                      
          ;
adtFields1 : adtField                       
           | adtField '|' adtFields1        
           ;
adtField : type_                            
         | null                             
         ;


ADTFieldsA : ADTFields1                     
           | Indent ADTFieldsN Dedent             
           ;
ADTFieldsN : ADTField Newline                   
           | ADTField Newline ADTFieldsN        
           ;
ADTFields1 : ADTField                       
           | ADTField '|' ADTFields1        
           ;
ADTField : ident '(' types ')'              
         | type_                            
         | null                             
         ;
