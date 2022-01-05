{ nixpkgs ? import ./nixpkgs.nix
}:

let
pkgs = import nixpkgs {};
ghc921 = pkgs.haskell.packages.ghc921.override {
  packageSetConfig = import ./hs-overlay.nix;
};

in { inherit (ghc921) pipes-zlib; }
