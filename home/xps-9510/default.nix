{ pkgs, username, inputs, config, nixpkgs-unstable, lib, ... }:


{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      nix-bubblewrap = inputs.nix-bubblewrap;
      pkgs-unstable = nixpkgs-unstable;
      sops = config.sops;
    };
    users.${username} = {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        # inputs.nix-bubblewrap.nix-bubblewrap.x86_64-linux
        ./modules/bottom
        ./modules/cli
        ./modules/config.nix
        # ./modules/emacs
        ../common/emacs
        ./modules/email
        ./modules/firefox
        ./modules/fish
        ./modules/git.nix
        ./modules/gtk-qt.nix
        ../common/kitty
        ./modules/languages.nix
        ./modules/lf.nix
        ./modules/packages.nix
        ./modules/services.nix
        ./modules/wayland.nix
        ./modules/zathura.nix
      ];
      programs.home-manager.enable = true;
      home = {
        stateVersion = "22.05";
        homeDirectory = "/home/${username}";
        username = "${username}";
        persistence."/persist/home/${username}" = {
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
            "org"
            "HealthData"

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
            ".config/calibre"

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
            ".cache/mu"

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
            ".config/hypr/config.conf"

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
          "text/plain" = "vim.desktop";
          "text/html" = "firefox";
          "x-scheme-handler/http" = "firefox";
          "x-scheme-handler/https" = "firefox";
        };
      };
    };
  };
}

