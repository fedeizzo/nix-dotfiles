{
  flake.modules.darwin.home-manager = { inputs, username, emacs-pkg, pkgs-unstable, ... }: {
    imports = [
      inputs.home-manager.darwinModules.home-manager
    ];
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit username inputs emacs-pkg pkgs-unstable;
      };
      backupFileExtension = "backup";
    };
  };
}
