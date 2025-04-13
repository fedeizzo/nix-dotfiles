{ username
, inputs
, config
, emacs-pkg
, pkgs-old
, pkgs-unstable
, anytype-pkgs
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
      inherit pkgs-old;
      inherit pkgs-unstable;
      inherit anytype-pkgs;
    };
    backupFileExtension = "to_delete";
    sharedModules = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];

    users.${ username} = {
      imports = [
        ../common/bottom
        ./modules/cli
        ../common/firefox
        ./modules/fish
        ../common/git
        ../common/languages
        ../common/nix-index
        ./modules/misc
        ./modules/user
        ./modules/plasma
        ./modules/stylix
        ../common/zathura
        ./modules/zed

        ../common/kitty
        # ../common/emacs
      ];
      programs. home-manager. enable = true;

      home = {
        stateVersion = "24.11";
        homeDirectory = "/home/${username}";
        username = "${username}";
      };

      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
    };
  };
}
