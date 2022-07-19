{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = { self, nixpkgs, hyprland, nixos-hardware, ... }@inputs:
    {
      # by default the configuration used for nixos-rebuild switch
      # is matched with the current hostname
      nixosConfigurations.fedeizzo-nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        specialArgs = { inherit inputs; };

        modules =
          let
            defaults = { pkgs, ... }: {
              _module.args.nixpkgs-unstable = import inputs.nixpkgs-unstable {
                inherit (pkgs.stdenv.targetPlatform) system;
              };
              _module.args.nixpkgs-old = import inputs.nixpkgs-old {
                inherit (pkgs.stdenv.targetPlatform) system;
              };
            };
          in
          ([
            defaults
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                inputs.emacs-overlay.overlay
              ];
            })
            ./system/boot.nix
            ./system/hardware.nix
            ./system/hardware-configurations/ext4.nix
            ./system/keymapTimeFont.nix
            ./system/pipewire.nix
            ./system/networking.nix
            ./system/nixNixOS.nix
            ./system/nvidia.nix
            ./system/programsEnv.nix
            ./system/security.nix
            ./system/services.nix
            ./system/user.nix
            hyprland.nixosModules.default
            { programs.hyprland.enable = true; }
          ]);
      };

      nixosConfigurations.rasp-nixos = nixpkgs.lib.nixosSystem {
        modules = [
          nixos-hardware.nixosModules.raspberry-pi-4
          ./raspberry/nixos.nix
          ./raspberry/hardware-configuration.nix
        ];
      };
    };
}
