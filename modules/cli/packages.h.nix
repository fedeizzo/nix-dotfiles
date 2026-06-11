{ pkgs, lib, ... }: {
  flake.modules.homeManager.cli-packages = {
    home.packages = with pkgs; [
      eza
      fd
      fzf
      gawk
      gnused
      bat
      ripgrep
      pandoc
      graphviz
      ffmpeg
      home-manager
      git-crypt
      rbw
      pet
      hurl
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      zip
      unzip
      bitwarden-cli
      jq
      coreutils
      moreutils
      gnutls
      rsync
      rclone
      dragon-drop
      w3m
      file
      yubioath-flutter
      cachix
      openssl
      openssh
      pamixer
      playerctl
      mpd-mpris
      flac
      (makeDesktopItem {
        name = "reboot";
        exec = "reboot";
        comment = "Reboot the system";
        desktopName = "reboot";
        type = "Application";
      })
      (makeDesktopItem {
        name = "shutdown";
        exec = "shutdown now";
        comment = "Shutdown the system";
        desktopName = "shutdown";
        type = "Application";
      })
    ];
  };
}
