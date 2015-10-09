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

doc: sourcegraph haddock

sourcegraph:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph HAHP.cabal

haddock:
	cabal haddock --executables
