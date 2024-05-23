{ pkgs, pkgs-unstable, ... }:

{
  imports = [ ../../../common/cli ];
  home.packages = with pkgs; [
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
    coreutils
    xdragon
    gnutls
    htop
    nmap
    powertop
    unzip
    xsv
    zsh
    cachix
    openssl
    rbw
    nix-index
    rsync
    brightnessctl
    jq
    openssh
    pamixer
    borgbackup
    ncdu
    playerctl
    libsecret
    w3m
    rclone
    sshfs
    flac
    file
    mpd-mpris
    moreutils
    zip
    pkgs-unstable.bitwarden-cli
  ];
}
