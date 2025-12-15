{
  description = "My personal NixOS configuration";

  inputs = {
    # Nixpkgs
    ##X1
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    solaar = {
      url = "github:Svenum/Solaar-Flake/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Mac
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    ## Homelab
    nixpkgs-homelab.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-homelab-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    climbing-lab.url = "path:/home/oven/docs/climbing-lab";

    # Flake management
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
    nix-topology.url = "github:oddlama/nix-topology";

    # Installation and boot
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Remote deployment and secretes
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";

    # Misc
    emacs-pkg.url = "github:nixos/nixpkgs/b805fe3d6f3e702ecee01710ee552e3ed39d16c8";
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";
    nh.url = "github:nix-community/nh";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    authentik-nix.url = "github:nix-community/authentik-nix";

    # Wayland and Hyprland
    stylix.url = "github:danth/stylix";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        imports = [
          inputs.devshell.flakeModule
          inputs.git-hooks-nix.flakeModule
          inputs.nix-topology.flakeModule
        ];
        systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
        perSystem = { pkgs, ... }: {
          formatter = pkgs.nixpkgs-fmt;
          imports = [
            ./nix/git-hooks.nix
            ./nix/devshells.nix
            ./nix/topology.nix
          ];
        };
        flake = {
          imports = [
            (import ./nix/nixosConfigurations.nix { inherit inputs; })
            (import ./nix/deployment.nix {
              inherit inputs;
              homelab-configuration = (import ./nix/nixosConfigurations.nix { inherit inputs; }).nixosConfigurations.homelab;
              rasp-configuration = (import ./nix/nixosConfigurations.nix { inherit inputs; }).nixosConfigurations.rasp;
            })
          ];
          checks = builtins.mapAttrs (_system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
        };
      };
}
