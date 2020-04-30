run: Main
	./Main

test: Main
	./Main test.bo -v

Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs CmpBuilder.hs Cmp.hs
	ghc -package haskeline -package llvm-hs-pure -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
