{
  flake-file.inputs.home-manager.url = "github:nix-community/home-manager/release-26.05";
  flake-file.inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  flake.modules.nixos.user = { inputs, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
    ];

    users.mutableUsers = false;
    programs.fuse.userAllowOther = true;
  };
}
