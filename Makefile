all: main

main: *.hs 
	ghc -Wall -ilib main.hs

clean:
	rm -f *.o *.hi lib/*.o lib/*.hi main

