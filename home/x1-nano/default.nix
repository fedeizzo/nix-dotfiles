{ username
, inputs
, config
, emacs-pkg
, pkgs-unstable
, ...
}:


{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit (config) sops;
      inherit username;
      inherit inputs;
      inherit emacs-pkg;
      inherit pkgs-unstable;
    };
    backupFileExtension = "to_delete";
    sharedModules = [ inputs.plasma-manager.homeModules.plasma-manager ];

    users.${username} = {
      imports = [
        ../common/bottom
        ./modules/cli
        ../common/firefox
        ./modules/fish
        ../common/git
        ../common/languages
        ../common/nix-index
        ./modules/misc
        ./modules/nextcloud
        ./modules/user
        ./modules/plasma
        ./modules/stylix
        ../common/zathura
        ./modules/zed

        ../common/kitty
        # ../common/emacs
      ];
      programs.home-manager.enable = true;

      home = {
        stateVersion = "25.05";
        homeDirectory = "/home/${username}";
        username = "${username}";
      };

      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
    };
  };
}
