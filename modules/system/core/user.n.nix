{
  flake.modules.nixos.user = { inputs, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
    ];

    users.mutableUsers = false;
    programs.fuse.userAllowOther = true;
  };
}
