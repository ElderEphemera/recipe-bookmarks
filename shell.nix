{ pkgs ? import nix/pinned-nixpkgs.nix }:
let
  spago2nix = import (builtins.fetchTarball {
    name = "spago2nix";
    url = "https://github.com/justinwoo/spago2nix/tarball/874efd1d8e25521463d3f6e60493e629f2ab1cd5";
    sha256 = "0cv6jm3wib1fpkni7swajv995wmbvxv969l4g19m6s9lj2rsbv05";
  }) { inherit pkgs; };

in pkgs.mkShell {
  packages = with pkgs; [ purescript spago pscid spago2nix ];
}
