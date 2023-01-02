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
    pkgs ? import <nixpkgs> { inherit config; }
  }:

  pkgs.mkShell rec {
    buildInputs = [
      pkgs.cabal-install
      pkgs.haskell.compiler.ghc902
      pkgs.haskellPackages.hoogle
      pkgs.haskell-language-server
      pkgs.llvmPackages_12.llvm
    ];

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
  }
