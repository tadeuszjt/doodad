test: lexer
	cabal run doodad -- std/test/test
	cabal run doodad -- lang/test/testLexer

lexer: bin/lexer
bin/lexer: lang/lexer.doo lang/lexerMain.doo
	mkdir -p bin
	cabal run doodad -- lang/lexerMain -c --use-old-lexer
	gcc build/chars.o build/strings.o build/io.o build/lexer.o build/lexerMain.o -o bin/lexer -lm -lgc
