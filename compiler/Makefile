build: 
	nix-build pkgs.nix -A hsPkgs.mimsa.components.exes.mimsa --quiet | cachix push mimsa

run:
	make build
	./result/bin/mimsa

build-test:
	nix-build pkgs.nix -A hsPkgs.mimsa.components.tests.mimsa-test --quiet | cachix push mimsa

unit-test:
	make build-test
	./result/bin/mimsa-test

hoogle:
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8090

haddock:
	stack build --fast --haddock-deps
	stack haddock --open
