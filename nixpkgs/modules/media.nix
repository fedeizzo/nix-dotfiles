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
    # spotifyd
    # spotify-tui
  ];

  # ZATHURA
  home.file."./.config/zathura/zathurarc" = {
    source = ../../dotfiles/dot_config/private_zathura/executable_zathurarc;
    executable = true;
  };

  # IMGUR-SCREENSHOT
  # xdg.configFile."imgur-screenshot/credentials.conf".source = ../../dotfiles/dot_config/imgur-screenshot/private_credentials.conf.tmpl;
  # xdg.configFile."imgur-screenshot/settings.conf".source = ../../dotfiles/dot_config/imgur-screenshot/private_settings.conf.tmpl;
}
