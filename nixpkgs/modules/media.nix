{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    feh
    ffmpeg
    flac
    flameshot
    imgur-screenshot
    maim
    mpv
    streamlink
    sxiv
    vlc
    file
    spotify
    mpd-mpris
    qrcp
  ];
}
