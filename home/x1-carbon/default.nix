{ username, inputs, config, emacs-pkg, pkgs-old, ... }:


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
    };
    backupFileExtension = "to_delete";
    sharedModules = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];

    users.${username} = {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence

        ../common/bottom
        ./modules/cli
        ../common/firefox
        ./modules/fish
        ../common/git
        ../common/languages
        ../common/nix-index
        ./modules/misc
        ./modules/persistent
        ./modules/user
        # ./modules/wayland
        ./modules/plasma
        ./modules/stylix
        # ../common/wezterm
        ../common/yazi
        ../common/zathura

        ../common/kitty
        ../common/emacs
      ];
      programs.home-manager.enable = true;

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
