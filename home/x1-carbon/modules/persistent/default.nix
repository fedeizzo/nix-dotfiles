{ username, ... }:

{
  home.persistence."/persist/home/${username}" = {
    allowOther = true;
    directories = [
      # Personal files
      ".gnupg"
      ".ssh"
      "docs"
      "nix-dotfiles"
      # "org"
      # "zettelkasten"

      # AppData
      ".local"

      # Config
      ".config/Bitwarden"
      ".config/Bitwarden CLI"
      ".config/borg"
      ".config/chromium"
      ".config/cni"
      ".config/emacs"
      ".config/pulse"
      ".config/rbw"
      ".config/rclone"
      # ".config/calibre"

      # Cache
      ".cargo"
      ".mozilla"
      ".cache/chromium"
      ".cache/emacs"
      ".cache/mozilla/firefox"
      ".cache/nix"
      ".cache/nix-index"
      ".cache/rbw"
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
}

