let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/cc54fb41d13736e92229c21627ea4f22199fee6b.tar.gz") {};
  ghcVersion = "ghc965";
  ghcWithPackages = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (pkgs: with pkgs; [
    haskell-language-server
  ]);
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    ghcWithPackages
    stack
    zlib.dev
  ];
}
