run: Main
	./Main
	llc main.ll
	gcc -no-pie main.s
	./a.out || true


Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs
	ghc Lexer.hs Main.hs Parser.hs AST.hs Compiler.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
