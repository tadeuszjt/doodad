run: Main
	./Main

test: Main
	rm test.bo.ll || true
	./Main test.bo -v
	llc test.bo.ll
	clang test.bo.s
	./a.out || true

Main: Main.hs Lexer.hs Parser.hs AST.hs Compiler.hs CmpVal.hs CmpBuilder.hs Cmp.hs JIT.hs
	ghc -package haskeline -package llvm-hs-pure -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
