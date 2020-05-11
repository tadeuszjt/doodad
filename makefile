run: Main
	./Main

test: Main
	./Main < test.bo

Main: Main.hs Lexer.hs Parser.hs AST.hs CmpAST.hs CmpValue.hs CmpFuncs.hs CmpMonad.hs JIT.hs
	ghc -package haskeline -package llvm-hs-pure -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
