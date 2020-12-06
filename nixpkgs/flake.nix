{
  description = "Home manager flake configuration";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.neovim-nightly-overlay.url = "github:mjlbach/neovim-nightly-overlay/flakes";

  inputs.home-manager = {
    url = "github:rycee/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, ... }@inputs:
    let
      nixos-unstable-overlay = final: prev: {
        nixos-unstable = import inputs.nixos-unstable {
          system = prev.system;
          # config.allowUnfree = true;
          overlays = [ inputs.noevim-nightly-overlay.overlay ];
        };
      };
      additional-package-overlay = final: prev: {
        # LS_COLORS = inputs.LS_COLORS;
      };
    in
    {
      homeConfigurations = {
        linux = inputs.home-manager.lib.homeManagerConfiguration {
          configuration = { pkgs, ... }:
            {
              nixpkgs.overlays = [
                nixos-unstable-overlay
                inputs.neovim-nightly-overlay.overlay
              ];
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
