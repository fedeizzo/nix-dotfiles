{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    feh
    ffmpeg
    flac
    flameshot
    mpv
    sxiv
    vlc
    zathura
    streamlink
  ];
}
