site: spago-packages.nix
	nix-build

spago-packages.nix: spago.dhall packages.dhall
	nix-shell --run "spago2nix generate"

.PHONY: site
