run: Main
	./Main

Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs CmpState.hs
	ghc -package haskeline -package llvm-hs-pure -package mtl Lexer.hs Main.hs Parser.hs AST.hs Compiler.hs CmpState.hs -outputdir build

Lexer.hs: Lexer.x
	~/.cabal/bin/alex Lexer.x

Parser.hs: Parser.y
	~/.cabal/bin/happy Parser.y
