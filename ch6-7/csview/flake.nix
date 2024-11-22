{
  description = "CSView by Learn Haskell by Example";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems f;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = final: _: {
        csview = final.haskellPackages.callCabal2nix "csview" ./. { };
      };

      packages = forAllSystems (system: {
        inherit (nixpkgsFor.${system}) csview;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.csview);

      checks = self.packages;

      devShell = forAllSystems (system:
        let inherit (nixpkgsFor.${system}) haskellPackages;
        in haskellPackages.shellFor {
          packages = _: [ self.packages.${system}.csview ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
