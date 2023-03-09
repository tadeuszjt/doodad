bench: lexer
	cabal run doodad -- std/test/test --print-llir | wc -l              # 4647
	cabal run doodad -- lang/lexer/test/testLexer --print-llir | wc -l  # 5599
	cabal run doodad -- lang/test/testLang --print-llir | wc -l         # 1392


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
	make new_lexer


new_lexer: bin/lexer lang/lexer/ clean
	cabal run doodad -- lang/lexer/lexerMain -c 
	gcc build/*.o -o bin/lexer -lgc -lm


clean:
	rm -f build/*.o


qbe_test: clean
	cabal run doodad -- lang/qbegen/qbegen -c
	rm -f lang/qbegen/qbe_test.ssa
	rm -f lang/qbegen/qbe_test.o
	rm -f lang/qbegen/qbe_test.s
	rm -f bin/qbe_test
	gcc build/*.o -o bin/qbegen -lgc -lm
	(./bin/qbegen > lang/qbegen/qbe_test.ssa || true)
	cat lang/qbegen/qbe_test.ssa
	qbe lang/qbegen/qbe_test.ssa > lang/qbegen/qbe_test.s
	gcc -c lang/qbegen/qbe_test.s -o lang/qbegen/qbe_test.o
	gcc lang/qbegen/qbe_test.o -o bin/qbe_test
	./bin/qbe_test


full_reset:
	rm -f build/*
	rm -f bin/*
	rm -rf dist-newstyle


