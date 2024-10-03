{ username, inputs, config, ... }:


{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      sops = config.sops;
      username = username;
      inputs = inputs;
    };

    users.${username} = {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence

        ../common/modules/bottom
        ./modules/cli
        ../common/modules/firefox
        ./modules/fish
        ./modules/git
        ./modules/languages
        ./modules/lf
        ./modules/misc
        ./modules/persistent
        ./modules/user
        ./modules/wayland
        ./modules/zathura


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

