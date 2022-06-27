{
  description = "Home manager flake configuration";

  # inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    # inputs.neovim-nightly-overlay.url = "github:mjlbach/neovim-nightly-overlay";
    neovim-nightly-overlay = {
      url = "github:neovim/neovim/nightly?dir=contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    wayplover = {
      url = "github:TravisDavis-ops/wayplover";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:rycee/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, ... }@inputs:
    let
      unstable-overlay = final: prev: {
        unstable = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
      };
      overlays = [
        inputs.neovim-nightly-overlay.overlay
        inputs.emacs-overlay.overlay
        unstable-overlay
        (final: prev: {
          swayhide = final.callPackage ./pkgs/swayhide.nix { };
          swaync = final.callPackage ./pkgs/swaync.nix { };
        })
        (self: super: {
          waybar = super.waybar.overrideAttrs (oldAttrs: {
            mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
          });
        })
      ];
      allowUnfree = { nixpkgs.config.allowUnfree = true; };
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
                allowUnfree
              ];
              # home.packages = [
              #   wayplover.defaultPackage
              # ];
            };
          system = "x86_64-linux";
          homeDirectory = "/home/fedeizzo";
          username = "fedeizzo";
          stateVersion = "22.05";
        };
      };
      linux = self.homeConfigurations.linux.activationPackage;
    };
}
