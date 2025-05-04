{
  description = "tuya";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forallSystems =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          })
        );
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hpkgsFor =
        system: pkgs:
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc910.override {
          overrides = self: super: {
            net-mqtt = doJailbreak super.net-mqtt;
            postgresql-libpq = doJailbreak super.postgresql-libpq;
            websockets = doJailbreak super.websockets;
          };
        };
    in
    {
      packages = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          tuya = haskellPackages.callCabal2nix "tuya" ./. { };
          default = self.packages.${system}.tuya;
        }
      );
      devShells = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          tuya = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.tuya ];
            buildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
          default = self.devShells.${system}.tuya;
        }
      );
    };
}
