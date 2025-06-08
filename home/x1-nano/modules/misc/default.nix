{ pkgs, anytype-pkgs, ... }:

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
    orca-slicer
    freecad
    openscad

    # notes
    anytype-pkgs.anytype

    element-desktop
  ];
}
