{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    bitwarden
    bitwarden-cli
    firefox
    qutebrowser
    libreoffice
    (nerdfonts.override { fonts = [ "Meslo"]; })
    openssh
    rofi
    haskellPackages.greenclip
    arandr
    spotify
    # simplescreenrecorder
    # steam
  ];
}

