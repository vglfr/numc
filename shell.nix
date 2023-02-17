let
  config = {
    packageOverrides = pkgs: {
      haskell-language-server = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "90" ];
      };
    };
  };

in
  {
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/724bfc0892363087709bd3a5a1666296759154b1.tar.gz") { inherit config; }
  }:

  pkgs.mkShell rec {
    buildInputs = [
      pkgs.cabal-install
      pkgs.haskell.compiler.ghc902
      pkgs.haskellPackages.hoogle
      pkgs.haskell-language-server
      pkgs.libgccjit
      pkgs.llvmPackages_12.llvm
    ];

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
  }
