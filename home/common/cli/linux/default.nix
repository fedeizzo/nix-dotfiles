{ pkgs, ... }:

{
  imports = [
    ../default.nix
  ];

  home.packages = with pkgs; [
    # rofi entries
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

    # cli
    zip
    unzip
    rbw
    bitwarden-cli
    jq

    # utils
    coreutils
    moreutils
    gnutls
    rsync
    rclone
    xdragon
    w3m
    file

    # nix
    cachix
    nix-index

    # linux
    openssl
    openssh


    # probably to remove
    # brightnessctl
    # libsecret

    # audio
    pamixer
    playerctl
    mpd-mpris
    flac
  ];

}
