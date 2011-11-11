all: main

main: main.hs Profile.o
	ghc -ilib main.hs

Profile.o: Profile.hs
	ghc -ilib Profile.hs

clean:
	rm -f *.o *.hi lib/*.o lib/*.hi main

