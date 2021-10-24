{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    flac
    flameshot
    mpv
    streamlink
    vlc
    file
    spotify
    mpd-mpris
    qrcp
  ];
}
