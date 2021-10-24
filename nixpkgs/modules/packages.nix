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
    bitwarden
    bitwarden-cli
    brightnessctl
    firefox
    xcmenu
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
    gnome3.gnome-keyring
    libsecret
    w3m
    qgis
    gimp
    calibre
    libreoffice
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
