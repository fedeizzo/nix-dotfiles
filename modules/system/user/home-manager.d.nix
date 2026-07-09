{
  flake-file.inputs.mac-app-util.url = "github:hraban/mac-app-util";

  flake.modules.darwin.home-manager = { inputs, username, pkgs-unstable, ... }: {
    imports = [
      inputs.home-manager.darwinModules.home-manager
      inputs.mac-app-util.darwinModules.default
    ];
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      sharedModules = [
        inputs.mac-app-util.homeManagerModules.default
      ];
      extraSpecialArgs = {
        inherit username inputs pkgs-unstable;
      };
      backupFileExtension = "backup";
    };
  };
}
