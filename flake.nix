{
  description = "My personal NixOS/HomeManager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    neovim-nightly-overlay.url = "github:mjlbach/neovim-nightly-overlay";

    home-manager = "github:nix-community/home-manager"
  }

  outputs = inputs@{ self, home-manger, nixpkgs, }:
    {
      system = "x84_64-linux";
      nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = ([
          # list of nix files.
          ./system/system.nix

          # home-manager stuff
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.fedeizzo = import ./home.nix
          }
        ]);
      }
    }

}
