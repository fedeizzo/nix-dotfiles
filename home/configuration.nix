{ pkgs, config, inputs, nixpkgs-unstable, lib, ... }:

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
        persistence."/persist/home/${config.username}" = {
          allowOther = true;
          directories = [
            # Personal files
            ".gnupg"
            ".ssh"
            "docs"
            "nix-dotfiles"
            "fbk"
            "uni"
            "personalProject"
            "zettelkasten"
            "org"

            # AppData
            ".local"

            # Config
            ".config/Bitwarden"
            ".config/Bitwarden CLI"
            ".config/borg"
            ".config/chromium"
            ".config/cni"
            ".config/draw.io"
            ".config/emacs"
            ".config/google-chrome"
            ".config/libvirt"
            ".config/pulse"
            ".config/rbw"
            ".config/rclone"
            ".config/spotify"

            # Cache
            ".cargo"
            ".mozilla"
            ".cache/borg"
            ".cache/chromium"
            ".cache/emacs"
            ".cache/google-chrome"
            ".cache/lf"
            ".cache/mozilla/firefox"
            ".cache/nix"
            ".cache/nix-index"
            ".cache/nvidia"
            ".cache/org-persist"
            ".cache/rbw"
            ".cache/spotify"

            # I need to understand if following folders are required
            # ".dbus"
            # ".esd_auth"
            # ".cache/dconf"
            # ".cache/fontconfig"
            # ".config/GIMP"
            # ".config/inkscape"
          ];
          files = [
            # Personal files
            ".aspell.en.prepl"
            ".aspell.en.pws"
            ".local/share/fish/fish_history"
            ".local/share/z/data"

            # Config
            ".config/user-dirs.dirs"
            ".config/user-dirs.locale"

            # Cache
            ".cache/rofi-2.sshcache"
            ".cache/rofi-3.runcache"
            ".cache/rofi3.druncache"
            ".cache/rofi3.filebrowsercache"
          ];
        };
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

