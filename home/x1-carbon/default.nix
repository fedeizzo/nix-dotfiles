{ username, inputs, config, ... }:


{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupfileextension = "backup";
    extraspecialargs = {
      sops = config.sops;
      username = username;
      inputs = inputs;
    };

    users.${username} = {
      imports = [
        inputs.impermanence.nixosmodules.home-manager.impermanence

        ../common/bottom
        ./modules/cli
        ../common/firefox
        ./modules/fish
        ../common/git
        ../common/languages
        ./modules/misc
        ./modules/persistent
        ./modules/user
        ./modules/wayland
        ../common/zathura


        ../common/kitty
        ../common/emacs
      ];
      programs.home-manager.enable = true;

      home = {
        stateVersion = "23.11";
        homeDirectory = "/home/${username}";
        username = "${username}";
      };

      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
    };
  };
}

