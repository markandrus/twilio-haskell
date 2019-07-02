{ compiler ? "ghc865" }:

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs stdenv;
in
# Make a new "derivation" that represents our shell
stdenv.mkDerivation {
  name = "twilio-haskell";

  # The packages in the `buildInputs` list will be added to the PATH in our shell
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.${compiler}
    pkgs.xz
    pkgs.zlib
  ];

  LIBRARY_PATH = "${pkgs.xz.out}/lib:${pkgs.zlib}/lib";
}
