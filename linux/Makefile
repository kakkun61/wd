wd: main.c
	$(CC) -Wall -O -o wd main.c

.PHONY: install
install: wd
	install -D -m 755 wd $(out)/bin/wd

.PHONY: format
format:
	clang-format -i -style=file main.c
	nixpkgs-fmt default.nix flake.nix

.PHONY: clean
clean:
	-$(RM) wd
