syn keyword doodadKeyword return 
syn keyword doodadConditional if else switch
syn keyword doodadFunc fn type feature aquires enum tuple
syn keyword doodadDef let data const in
syn keyword doodadLoop for while
syn keyword doodadTop module import link include
syn keyword doodadType U8 I8 I16 I32 I64 F32 F64 Bool Char String Table conv
syn keyword doodadConst true false
syn match   doodadInt '\<\d\+\>'
syn match   doodadChar '\'.\''
syn match   doodadSpecial "->"
syn match   doodadString  '\"[^"]*\"'
syn match   doodadComment '//.*'
syn match   doodadType "\<[A-Z][a-zA-Z0-9_]*\>"


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
