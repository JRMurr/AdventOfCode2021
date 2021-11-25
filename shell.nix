{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [ pkgs.haskell.compiler.ghc8107 pkgs.cabal-install ];
}
