{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    coreutils
    dragon-drop
    exa
    fd
    fzf
    gawk
    gnused
    gnutls
    gotop
    htop
    nmap
    powertop
    universal-ctags
    unzip
    xsv
    zsh
    cachix
    bat
    ripgrep
    openssl
    rbw
    pinentry-qt
    nix-index
    rsync
    (nerdfonts.override {
      fonts = [
        "Meslo"
        "RobotoMono"
      ];
    })
    unstable.bitwarden
    unstable.bitwarden-cli
    brightnessctl
    firefox
    jq
    keyutils
    openssh
    pamixer
    pandoc
    pavucontrol
    playerctl
    qutebrowser
    borgbackup
    ncdu
    thunderbird
    joypixels
    libsecret
    w3m
    qgis
    gimp
    calibre
    # libreoffice
    google-chrome
    rclone
    lazygit
    # unstable.qmk
    chessx
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
  };
}
