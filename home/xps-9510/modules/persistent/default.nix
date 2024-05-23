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

