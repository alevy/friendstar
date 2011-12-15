all: main

main: *.hs */*.hs */**/*.hs
	ghc -Wall -ilib main.hs

clean:
	rm -f *.o *.hi lib/*.o lib/*.hi lib/**/*.o lib/**/*.hi Views/*.o Views/*.hi Controllers/*.o Controllers/*.hi main Extensions/*.hi Extensions/*.o

run: main
	./main

