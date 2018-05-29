.PHONY: shell build build_nix test clean hoogle tags

# Create a shell with dependencies.
shell:
	nix-shell -A shells.ghcjs

## Run in a shell.
#build:
#	cabal new-build all

build_nix:
	nix-build

## Run in a shell.
#test:
#	cabal new-test

#clean:
#	cabal clean

# Run in a shell.
hoogle:
	hoogle server -p 8080 --local

tags:
	hasktags -c src
