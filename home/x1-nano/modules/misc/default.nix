{ pkgs, pkgs-unstable, ... }:

{
  home.packages = with pkgs; [
    moonlight-qt
    pavucontrol
    vlc
    bitwarden
    tdesktop
    ddcui # control external monitor
    ddcutil

    # 3d printing
    bambu-studio
    freecad
    openscad

    # notes
    pkgs-unstable.anytype
  ];
}
