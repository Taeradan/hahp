EXECUTABLE:=dist/build/hahp-example/hahp-example +RTS -lf -N2 -l

run:	build
	date
	$(EXECUTABLE) > out.md

build:
	cabal build

pdf: run
	pandoc out.md -o out.pdf -V geometry:a4paper -V geometry:margin=2cm
	date

#---- Improvement

hlint:
	hlint -r src

profile: run
	threadscope hahp-example.eventlog

stylish-haskell:
	find src -name "*.hs" -exec stylish-haskell -i {} \;

#---- Documentation

doc:	sourcegraph haddock
.PHONY: sourcegraph haddock

haddock:
	cabal haddock

sourcegraph:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph hahp.cabal

