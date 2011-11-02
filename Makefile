all: main

main: main.hs
	ghc -ilib main.hs

clean:
	rm *.o *.hi main
