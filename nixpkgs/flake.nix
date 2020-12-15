{
  description = "Home manager flake configuration";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.neovim-nightly-overlay.url = "github:mjlbach/neovim-nightly-overlay";

  inputs.home-manager = {
    url = "github:rycee/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.neuron-nightly = {
    url = "github:srid/neuron/nightly";
    flake = false;
  };

  outputs = { self, ... }@inputs:
    let
      overlays = [
        inputs.neovim-nightly-overlay.overlay 
        additional-package-overlay
      ];
      additional-package-overlay = final: prev: {
        neuron-nightly-overlay = inputs.neuron-nightly;
      };
    in
    {
      homeConfigurations = {
        linux = inputs.home-manager.lib.homeManagerConfiguration {
          configuration = { pkgs, ... }:
            {
              nixpkgs.overlays = overlays;
              nixpkgs.config = import ./laptop/config.nix;
              imports = [
                ./laptop/home.nix
              ];
            };
          system = "x86_64-linux";
          homeDirectory = "/home/fedeizzo";
          username = "fedeizzo";
        };
      };
      linux = self.homeConfigurations.linux.activationPackage;
    };
}
