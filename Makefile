test: lexer
	cabal run doodad -- std/test/test
	cabal run doodad -- lang/test/testLexer
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

new_lexer: bin/lexer lang/lexerMain.doo lang/lexer.doo
	cabal run doodad -- lang/lexerMain -c 
	gcc build/*.o -o bin/lexer -lgc -lm

fullReset:
	rm -f build/*
	rm -f bin/*
	rm -rf dist-newstyle


