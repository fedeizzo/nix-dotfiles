{ pkgs, ... }:

{
  home.packages = with pkgs; [
    moonlight-qt
    pavucontrol
    vlc
    bitwarden-desktop
    telegram-desktop
    ddcui # control external monitor
    ddcutil

    # 3d printing
    orca-slicer
    freecad
    openscad

    # notes
    pkgs.anytype

    element-desktop
  ];
}
