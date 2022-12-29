{
  pkgs ? import <nixpkgs> {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.clang
    pkgs.haskell.compiler.ghc902
    pkgs.llvmPackages_12.llvm
  ];
}
