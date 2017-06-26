all: plumber
plumber: plumber.g
	antlr -gt plumber.g
	dlg -ci parser.dlg scan.c
	g++ -w -std=c++11 -o plumber plumber.c scan.c err.c -I /usr/include/pccts
clean:
	rm plumber plumber.c err.c mode.h parser.dlg scan.c tokens.h
run:
	./plumber < inp.txt | ghci plumber.hs
