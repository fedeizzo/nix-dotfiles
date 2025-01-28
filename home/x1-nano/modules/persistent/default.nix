{ username, ... }:

{
  environment.persistence."/persist" = {
    hideMounts = true;
    users.${username} = {
      directories = [
        # Personal files
        ".gnupg"
        ".ssh"
        "docs"
        "nix-dotfiles"
        # "org"
        "notes"
        "zettelkasten"

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
        ".config/Moonlight Game Streaming Project"
        ".config/FreeCAD"
        ".config/BambuStudio"

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

        # plasma
        ".config/kde.org"
        ".config/plasma-workspace"
        ".config/xsettingsd"

        # ".local/share/baloo"
        # ".local/share/dolphin"
        # ".local/share/kactivitymanagerd"
        # ".local/share/klipper"
        # ".local/share/konsole"
        # ".local/share/kwalletd"
        # ".local/share/RecentDocuments"
        # ".local/share/sddm"
      ];
      files = [
        # Personal files
        ".aspell.en.prepl"
        ".aspell.en.pws"

        # Config
        ".config/user-dirs.dirs"
        ".config/user-dirs.locale"
        ".config/hypr/config.conf"

        # Cache
        ".cache/rofi-2.sshcache"
        ".cache/rofi-3.runcache"
        ".cache/rofi3.druncache"
        ".cache/rofi3.filebrowsercache"

        # plasma
        # ".config/baloofilerc"
        # ".config/gtkrc"
        # ".config/gtkrc-2.0"
        # ".config/kactivitymanagerdrc"
        # ".config/kcminputrc"
        # ".config/kconf_updaterc"
        # ".config/kded5rc"
        # ".config/kdeglobals"
        # ".config/kglobalshortcutsrc"
        # ".config/konsolerc"
        # ".config/konsolesshconfig"
        # ".config/krunnerrc"
        # ".config/kscreenlockerrc"
        # ".config/ksmserverrc"
        # ".config/ksplashrc"
        # ".config/ktimezonedrc"
        ".config/kwinoutputconfig.json"
        # ".config/kwinrc"
        # ".config/plasma-localerc"
        # ".config/plasma-org.kde.plasma.desktop-appletsrc"
        # ".config/plasmarc"
        # ".config/plasmashellrc"
        # ".config/powerdevilrc"
        # ".config/powermanagementprofilesrc"
        # ".config/systemsettingsrc"
      ];
    };
  };
}
