{
  description = "tuya";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # hlib = pkgs.haskell.lib;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: { };

        };
        hls = pkgs.haskell-language-server.override { dynamic = true; };
        packageName = "tuya";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self { };

        packages.default = self.packages.${system}.${packageName};

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.${packageName} ];
          buildInputs = [ haskellPackages.cabal-install hls ];
          withHoogle = true;
        };
      });
}
