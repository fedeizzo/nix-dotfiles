{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    universal-ctags
    pinentry-qt
    (nerdfonts.override {
      fonts = [
        "Meslo"
        "RobotoMono"
      ];
    })
    unstable.bitwarden
    firefox
    keyutils
    pavucontrol
    joypixels
    qgis
    gimp
    calibre
    onlyoffice-bin
    google-chrome
  ];
}
