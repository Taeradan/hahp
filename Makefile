SMALLCHECK_DEPTH:=5
QUICKCHECK_TESTS:=20000

# a remplacer par la découverte auto du nombre de coeurs
NPROCS:=8


EXECUTABLE:=.stack-work/install/x86_64-linux-nix/lts-7.15/8.0.1/bin/hahp-example +RTS -lf -N2 -l

run:	build
	date
	$(EXECUTABLE) | tee out.md

build:
	stack build

pdf: run
	pandoc out.md -o out.pdf -V geometry:a4paper -V geometry:margin=2cm
	date

test:
	@echo "--------------------------------------------------------------------"
	@#stack test || true
	@#stack test --test-arguments "--hide-successes" || true
	@stack test --test-arguments "-j$(NPROCS) --smallcheck-depth $(SMALLCHECK_DEPTH) --quickcheck-tests $(QUICKCHECK_TESTS) --hide-successes" || true
	@date

#---- Improvement

hlint:
	hlint -r src

profile: run
	threadscope hahp-example.eventlog

stylish-haskell:
	find src -name "*.hs" -exec stylish-haskell -c stylish-haskell.yaml -i {} \;

#---- Documentation

doc:	sourcegraph haddock
.PHONY: sourcegraph haddock

haddock:
	cabal haddock

sourcegraph:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph hahp.cabal

