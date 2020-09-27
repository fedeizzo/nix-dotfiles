{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Meslo" ]; })
    arandr
    bitwarden
    bitwarden-cli
    firefox
    haskellPackages.greenclip
    libreoffice
    openssh
    pandoc
    qutebrowser
    rofi
    spotify
    # simplescreenrecorder
    # steam
  ];
}

