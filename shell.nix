let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/1719f27dd95fd4206afb9cec9f415b539978827e.tar.gz") { };
  ghcVersion = "ghc965";
  ghcWithPackages = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (pkgs: with pkgs; [
    haskell-language-server
    hoogle
    cabal-install
    stack
  ]);
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    ghcWithPackages
    zlib.dev
  ];
}
