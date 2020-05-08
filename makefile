run: Main
	./Main

test: Main
	./Main -v < test.bo

Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs CmpVal.hs CmpBuilder.hs Cmp.hs JIT.hs
	ghc -package haskeline -package llvm-hs-pure -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
