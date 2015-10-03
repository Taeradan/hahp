run:
	cabal run

hlint:
	hlint src/*.hs

pdf:
	cabal install
	HAHP > out.md
	pandoc out.md -o out.pdf -V geometry:margin=1cm
	
doc:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph HAHP.cabal
	cabal haddock --executables
