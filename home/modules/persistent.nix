{ lib, config, home, ... }:

{
  # required to allow root to see bind of directories
  home.pesistance."/persist/home/${config.username}" = {
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
      ".cache/containers"
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
}

