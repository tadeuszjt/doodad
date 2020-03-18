Main: Main.hs Lexer.hs Parser.hs Compiler.hs
	ghc *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
