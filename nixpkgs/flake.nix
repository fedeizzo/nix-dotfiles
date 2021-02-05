{
  description = "Home manager flake configuration";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.neovim-nightly-overlay.url = "github:mjlbach/neovim-nightly-overlay";

  inputs.home-manager = {
    url = "github:rycee/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, ... }@inputs:
    let
      airflow-overlay = (self: super: {
        apache-airflow = super.apache-airflow.overrideAttrs (old: rec {
          name = "apache-airflow";
          version = "2.0.0";

          src = super.fetchFromGitHub rec {
            owner = "apache";
            repo = "airflow";
            rev = version;
            sha256 = "0000000000000000000000000000000000000000000000000000";
          };
        });
      });
      overlays = [
        inputs.neovim-nightly-overlay.overlay 
        airflow-overlay
      ];
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
