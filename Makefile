run:
	date
	cabal run

hlint:
	hlint -r src

stylish-haskell:
	find src -name "*.hs" -exec stylish-haskell -i {} \;

pdf:
	cabal build
	dist/build/HAHP/HAHP > out.md
	pandoc out.md -o out.pdf -V geometry:margin=1cm
	
doc:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph HAHP.cabal
	cabal haddock --executables
