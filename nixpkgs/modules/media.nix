{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    sxiv
    feh
    mpv
    vlc
    zathura
    ffmpeg
    flameshot
  ];
}
