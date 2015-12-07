CC = ghc

INCLUDE = 
LIB =

all: xmlSearch

xmlSearch: xmltree.hs Parser.hs
	$(CC) -o xmlSearch $(INCLUDE) xmltree.hs $(LIB)

clean:
	-rm *.o *~
	-rm xmlSearch
