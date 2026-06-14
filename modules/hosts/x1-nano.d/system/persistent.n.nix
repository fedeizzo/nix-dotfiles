{
  flake.modules.nixos.x1-nano = { username, ... }: {
    environment.persistence."/persist" = {
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/libvirt"
        "/var/lib/fprint"
        "/var/lib/dms-greeter"
      ];
      files = [
        "/root/.gitconfig"
      ];
      users.${username} = {
        directories = [
          # Personal files
          ".gnupg"
          ".ssh"
          "docs"
          "nix-dotfiles"
          "notes"
          "zettelkasten"
          "Documents"

          # AppData
          ".local"
          ".pi"
          ".gemini"

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
          ".config/Moonlight Game Streaming Project"
          ".config/FreeCAD"
          ".config/BambuStudio"
          ".config/OrcaSlicer"
          ".config/anytype"
          ".config/zed"
          ".zen"
          ".config/Nextcloud"
          ".config/libreoffice"
          ".config/niri"
          ".config/tailscale"

          # Cache
          ".cargo"
          ".mozilla"
          ".cache/chromium"
          ".cache/emacs"
          ".cache/mozilla/firefox"
          ".cache/nix"
          ".cache/nix-index"
          ".cache/rbw"
          ".cache/Moonlight Game Streaming Project"
          ".cache/FreeCAD"
          ".cache/bambu-studio"
          ".cache/orca-slicer"
          ".cache/zed"
          ".cache/zen"
          ".cache/Nextcloud"
          ".cache/DankMaterialShell"

          # plasma
          ".config/kde.org"
          ".config/plasma-workspace"
          ".config/xsettingsd"
        ];
        files = [
          # Personal files
          ".aspell.en.prepl"
          ".aspell.en.pws"

          # Config
          ".config/user-dirs.locale"
          ".config/hypr/config.conf"

          # Cache
          ".cache/rofi-2.sshcache"
          ".cache/rofi-3.runcache"
          ".cache/rofi3.druncache"
          ".cache/rofi3.filebrowsercache"

          ".config/kwinoutputconfig.json"
          ".config/codebook/codebook.toml" # spellcheck in zed
        ];
      };
    };
  };
}
