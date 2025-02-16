{
  description = "Evolutionary Iterated Prisoner's Dilemma";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        packageName = "evo-ipd";
        package = haskellPackages.callCabal2nix packageName ./. {};
      in
      {
        packages.default = package;
        packages.${packageName} = package;

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/${packageName}";
        };
        apps.${packageName} = self.apps.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ];
        };
      });
} 