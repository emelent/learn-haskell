with import <nixpkgs> {};

mkShell {
  buildInputs = [
    haskell.compiler.ghc8102
  ];
}
