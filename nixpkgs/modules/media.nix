{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    mpv
    streamlink
    vlc
    spotify
  ];
}
