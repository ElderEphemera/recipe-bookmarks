{ pkgs ? import <nixpkgs> {} }:
let
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
  depSources = pkgs.lib.strings.concatMapStringsSep " "
    (pkg: ''"${pkg}/src/**/*.purs"'')
    (builtins.attrValues spagoPkgs.inputs);

in pkgs.stdenv.mkDerivation {
  name = "recipe-bookmarks-script";
  src = pkgs.nix-gitignore.gitignoreSource [ ".git" ] ./.;
  buildInputs = [ pkgs.purescript ];

  buildPhase = ''
    purs compile "src/**/*.purs" ${depSources}
    purs bundle "output/**/*.js" --main Main --output script.js
  '';
  installPhase = ''
    mkdir $out
    cp index.html style.css script.js $out
  '';
}
