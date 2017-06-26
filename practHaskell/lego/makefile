all: prac haskell run softClean
prac: reader.g
	antlr -gt reader.g
	dlg -ci parser.dlg scan.c
	g++ -w -std=c++11 -o pccts reader.c scan.c err.c -I /usr/include/pccts
haskell: program.hs
	ghc --make program.hs
clean:
	rm pccts reader.c err.c mode.h parser.dlg scan.c tokens.h program program.hi reader.o
softClean:
	@rm reader.c err.c mode.h parser.dlg scan.c tokens.h program.hi reader.o 2> /dev/null || true
run:
	./pccts < input.txt > programhs.txt
	./program