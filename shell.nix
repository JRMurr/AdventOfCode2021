{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    # pkgs.haskell.compiler.ghc8107
    pkgs.haskell-language-server
    pkgs.cabal-install
  ];
}
