{
  flake.modules.darwin.home-manager = { inputs, username, pkgs-unstable, ... }: {
    imports = [
      inputs.home-manager.darwinModules.home-manager
    ];
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit username inputs pkgs-unstable;
      };
      backupFileExtension = "backup";
    };
  };
}
