all: build cptest

build: podcatcher.hs
	ghc -threaded $<

cptest: podcatcher
	cp $< testing

clean:
	rm podcatcher podcatcher.hi podcatcher.o 2>/dev/null || exit 0