run: Main
	./Main

test: Main
	rm test.bo.ll || true
	./Main test.bo -v
	clang-10 test.bo.ll
	./a.out || true

Main: Error.hs Main.hs Lexer.hs Parser.hs Type.hs AST.hs Resolver.hs Print.hs CmpAST.hs CmpADT.hs Table.hs Value.hs CmpFuncs.hs CmpMonad.hs JIT.hs
	ghc -package haskeline -package llvm-hs-pure -package llvm-hs -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
