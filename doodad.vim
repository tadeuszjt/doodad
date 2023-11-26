syn keyword doodadKeyword len print return 
syn keyword doodadConditional if else switch
syn keyword doodadFunc fn type
syn keyword doodadDef let data const in
syn keyword doodadLoop for while
syn keyword doodadTop module import link include
syn keyword doodadType i8 i16 i32 i64 f32 f64 bool char string table conv
syn keyword doodadConst true false null
syn match   doodadInt '\<\d\+\>'
syn match   doodadChar '\'.\''
syn match   doodadSpecial '{'
syn match   doodadSpecial '\}'
syn match   doodadSpecial "->"
syn match   doodadString  '\"[^"]*\"'
syn match   doodadComment '//.*'


hi def link doodadKeyword Statement
hi def link doodadFunc Function
hi def link doodadConditional Conditional
hi def link doodadLoop Repeat
hi def link doodadType Type
hi def link doodadTop PreProc
hi def link doodadInt Constant
hi def link doodadChar Character
hi def link doodadString String
hi def link doodadConst Constant
hi def link doodadDef Identifier
hi def link doodadSpecial Special
hi def link doodadComment Comment
