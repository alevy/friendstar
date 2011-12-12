all: main

main: *.hs lib/*.hs Views/*.hs Controllers/*.hs
	ghc -Wall -ilib main.hs

clean:
	rm -f *.o *.hi lib/*.o lib/*.hi Views/*.o Views/*.hi Controllers/*.o Controllers/*.hi main

