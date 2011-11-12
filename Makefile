all: main

main: 
	ghc -ilib main.hs

clean:
	rm -f *.o *.hi lib/*.o lib/*.hi main

