{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    coreutils
    xdragon
    exa
    fd
    fzf
    gawk
    gnused
    gnutls
    htop
    nmap
    powertop
    unzip
    xsv
    zsh
    cachix
    bat
    ripgrep
    openssl
    rbw
    rofi-rbw
    nix-index
    rsync
    brightnessctl
    jq
    openssh
    pamixer
    pandoc
    borgbackup
    ncdu
    playerctl
    libsecret
    w3m
    rclone
    lazygit
    graphviz
    sshfs
    ffmpeg
    flac
    file
    mpd-mpris
    moreutils
    home-manager
    zip
  ];
  xdg.configFile."rofi-rbw.rc" = {
    source = ./rofi-rbw.rc;
  };
}
