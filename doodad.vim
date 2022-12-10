syn keyword doodadKeyword push pop len clear print return 
syn keyword doodadConditional if else switch
syn keyword doodadFunc fn
syn keyword doodadDef let type data
syn keyword doodadLoop for while
syn keyword doodadTop module import import_c
syn keyword doodadType i8 i16 i32 i64 f32 f64 bool char string sparse
syn keyword doodadConst true false
syn match   doodadInt '\<\d\+\>'
syn match   doodadChar '\'.\''
syn match   doodadSpecial '{'
syn match   doodadSpecial '\}'
syn match   doodadSpecial "->"
syn match   doodadString  '\".*\"'



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
