{ pkgs, config, inputs, nixpkgs-unstable, ... }:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${config.username} = {
      imports = [
        inputs.hyprland.homeManagerModules.default
        inputs.impermanence.nixosModules.home-manager.impermanence
        ./modules/bottom.nix
        ./modules/cli.nix
        ./modules/config.nix
        ./modules/emacs.nix
        ./modules/fish.nix
        ./modules/git.nix
        ./modules/gtk-qt.nix
        ./modules/kitty.nix
        ./modules/languages.nix
        ./modules/lf.nix
        ./modules/neovim.nix
        ./modules/packages.nix
        # ./modules/qutebrowser.nix
        ./modules/services.nix
        ./modules/wayland.nix
        ./modules/zathura.nix
      ];
      programs.home-manager.enable = true;
      home = {
        stateVersion = "22.05";
        homeDirectory = "/home/${config.username}";
        username = "${config.username}";
        packages = with pkgs; [
          nixpkgs-unstable.nodePackages.pyright
          nixpkgs-unstable.bitwarden-cli
          nixpkgs-unstable.hledger
          nixpkgs-unstable.bitwarden
          nixpkgs-unstable.tdesktop
          nixpkgs-unstable.xournalpp
        ];
      };
      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "application/pdf" = "org.pwmt.zathura.desktop";
          "inode/directory" = "lf.desktop";
          "text/plain" = "nvim.desktop";
          "text/html" = "firefox";
          "x-scheme-handler/http" = "firefox";
          "x-scheme-handler/https" = "firefox";
        };
      };
    };
  };
}

