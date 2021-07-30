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
    xdotool
    nix-index
    rsync
    (nerdfonts.override {
      fonts = [
        "Meslo"
        "RobotoMono"
      ];
    })
    arandr
    bitwarden
    bitwarden-cli
    brightnessctl
    firefox
    xcmenu
    jq
    keyutils
    multilockscreen
    openssh
    pamixer
    pandoc
    pavucontrol
    playerctl
    qutebrowser
    xorg.xmodmap
    devour
    xss-lock
    borgbackup
    steam
    minecraft
    ncdu
    thunderbird
    joypixels
    trayer
    gnome3.gnome-keyring
    libsecret
    w3m
    qgis
    lutris
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
