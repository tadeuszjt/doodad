test: lexer
	cabal run doodad -- std/test/test
	cabal run doodad -- lang/lexer/test/testLexer
	cabal run doodad -- lang/test/testLang

lexer: bin/lexer
bin/lexer: bootstrap
	mkdir -p bin
	/usr/lib/llvm-9/bin/llc bootstrap/lexer/strings.ll
	/usr/lib/llvm-9/bin/llc bootstrap/lexer/io.ll
	/usr/lib/llvm-9/bin/llc bootstrap/lexer/chars.ll
	/usr/lib/llvm-9/bin/llc bootstrap/lexer/lexer.ll
	/usr/lib/llvm-9/bin/llc bootstrap/lexer/lexerMain.ll
	gcc -no-pie -s bootstrap/lexer/*.s -o bin/lexer -lgc -lm
	rm bootstrap/lexer/*.s

new_lexer: bin/lexer lang/lexer/ clean
	cabal run doodad -- lang/lexer/lexerMain -c 
	gcc build/*.o -o bin/lexer -lgc -lm



clean:
	rm -f build/*.o


qbe_test: clean
	cabal run doodad -- lang/qbegen/qbegen -c
	gcc build/*.o -o bin/qbegen -lgc -lm
	./bin/qbegen





full_reset:
	rm -f build/*
	rm -f bin/*
	rm -rf dist-newstyle


