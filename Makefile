PWSH = pwsh

.PHONY: build
build:
	cabal v2-build

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: format
format:
	stylish-haskell --inplace --recursive .

.PHONY: lint
lint:
	hlint .

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content .\Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
