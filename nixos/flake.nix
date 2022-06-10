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
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations.fedeizzo-nixos = nixpkgs.lib.nixosSystem {
        inherit system;

        specialArgs = { inherit system inputs; };

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
              nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
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
          ]);
      };
    };
}
