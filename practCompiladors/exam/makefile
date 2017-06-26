file = plumber

$(file): $(file).g
	antlr -gt $(file).g
	dlg -ci parser.dlg scan.c
	g++ -w -std=c++11 -o $(file) $(file).c scan.c err.c -I /usr/include/pccts
clean:
	rm $(file).c $(file) err.c mode.h parser.dlg scan.c tokens.h
run:
	./$(file) < inp.txt
