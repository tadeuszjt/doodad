Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs
	ghc Lexer.hs Main.hs Parser.hs AST.hs Compiler.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
