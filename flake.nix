{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    deploy-rs.url = "github:serokell/deploy-rs";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    hyprwm-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
    , deploy-rs
    , home-manager
    , impermanence
    , sops-nix
    , ...
    }@inputs:
    let
      lib = import ./lib { inherit inputs; };
      inherit (lib) mkHost forAllSystems;
    in
    rec {
      overlays = {
        emacs = inputs.emacs-overlay.overlay;
        default = import ./overlays { inherit inputs; };
      };
      legacyPackages = forAllSystems (system:
        import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      nixosConfigurations = {
        fedeizzo-nixos = mkHost {
          username = "fedeizzo";
          hostname = "fedeizzo-nixos";
          fs = "btrfs";
          system = "x86_64-linux";
          machine = "xps-9510";
          pkgs = legacyPackages."x86_64-linux";
        };
        rasp-nixos = mkHost {
          username = "rasp";
          hostname = "rasp-nixos";
          fs = "ext4";
          system = "aarch64-linux";
          machine = "raspberry";
          pkgs = legacyPackages."aarch64-linux";
        };
      };

      deploy.nodes.rasp-nixos = {
        hostname = "home-lab";
        sshUser = "root";
        sudo = "doas -u";
        sshOpts = [ ];
        magicRollback = true;
        autoRollback = true;
        fastConnection = false;
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.rasp-nixos;
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      templates = {
        python = {
          path = ./templates/python-mach-nix;
          description = "A white python mach-nix project";
        };
      };
    };
}


