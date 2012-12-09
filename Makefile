all: build

build: podcatcher.hs
	ghc -threaded podcatcher.hs

cptest: podcatcher
	cp $< testing

clean:
	rm podcatcher podcatcher.hi podcatcher.o 2>/dev/null || exit 0