{ pkgs, ... }:

{
  home.packages = with pkgs; [
    moonlight-qt
    pavucontrol
    vlc
    bitwarden
    tdesktop
  ];
}
