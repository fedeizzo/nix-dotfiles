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
    zathura
    file
    spotify
    mpd-mpris
    qrcp
  ];

  # ZATHURA
  home.file."./.config/zathura/zathurarc" = {
    source = ../dotfiles/dot_config/private_zathura/executable_zathurarc;
    executable = true;
  };
}
