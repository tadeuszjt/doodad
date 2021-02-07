run: Main
	./Main

test: Main
	./Main -v main < std/io.bo

Main: ADT.hs Args.hs Table.hs Funcs.hs Print.hs State.hs Value.hs Compile.hs Monad.hs Flatten.hs Modules.hs Error.hs Main.hs Lexer.hs Parser.hs Type.hs AST.hs JIT.hs
	ghc -lgc -package haskeline -package llvm-hs-pure -package llvm-hs -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
