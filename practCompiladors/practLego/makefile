all: lego
lego: lego.g
	antlr -gt lego.g
	dlg -ci parser.dlg scan.c
	g++ -w -std=c++11 -o lego lego.c scan.c err.c -I /usr/include/pccts
clean:
	rm lego lego.c err.c mode.h parser.dlg scan.c tokens.h
run:
	./lego < input.txt
