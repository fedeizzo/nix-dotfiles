{
  description = "My personal NixOS/HomeManager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/20.09";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
    in  
    {
      nixosConfigurations.fedeizzo-nixos = nixpkgs.lib.nixosSystem {
        inherit system;

        specialArgs = { inherit system inputs; };

        modules = ([
          ./system/boot.nix
          ./system/hardware.nix
          ./system/hardware-configurations/btrfs.nix
          ./system/keymapTimeFont.nix
          ./system/networking.nix
          ./system/nixNixOS.nix
          ./system/programsEnv.nix
          ./system/security.nix
          ./system/services.nix
          ./system/user.nix
        ]);
      };
    };
}
