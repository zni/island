all:
	mkdir -p bin
	ghc -isrc --make src/Main.hs -o bin/island

clean:
	rm src/*.o src/*.hi bin/island
