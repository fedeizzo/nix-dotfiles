{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    bspwmbar
    betterlockscreen
    mpd-mpris
    pavucontrol
    keyutils
    jq
  ];

  # BSPWM
  xdg.configFile."bspwm/pacwall.png".source = ../../dotfiles/dot_config/bspwm/pacwall.png;
  home.file."./.config/bspwm/bspwmrc" = {
    source = ../../dotfiles/dot_config/bspwm/executable_bspwmrc;
    executable = true;
  };
  # SXHKD
  xdg.configFile."sxhkd/sxhkdrc".source = ../../dotfiles/dot_config/sxhkd/sxhkdrc;

  # BETTERLOCKSCREEN
  xdg.configFile."betterlockscreenrc".source = ../../dotfiles/dot_config/betterlockscreenrc;

  # DUNST
  xdg.configFile."dunst/dunstrc".source = ../../dotfiles/dot_config/dunst/dunstrc;
  xdg.configFile."dunst/critical.png".source = ../../dotfiles/dot_config/dunst/critical.png;
  xdg.configFile."dunst/normal.png".source = ../../dotfiles/dot_config/dunst/normal.png;
  xdg.configFile."dunst/low.png".source = ../../dotfiles/dot_config/dunst/low.png;

  # LF
  xdg.configFile."lf/lfrc".source = ../../dotfiles/dot_config/lf/lfrc;

  # NEOFETCH
  xdg.configFile."noefetch/config.conf".source = ../../dotfiles/dot_config/neofetch/config.conf;

  # PICOM 
  xdg.configFile."picom/picom.conf".source = ../../dotfiles/dot_config/picom/picom.conf;

  # STARSHIP
  xdg.configFile."starship.toml".source = ../../dotfiles/dot_config/starship.toml;

  # Xresources
  home.file.".Xresources" = {
    source = ../../dotfiles/dot_Xresources;
  };

  # xinitrc
  home.file.".xinitrc" = {
    source = ../../dotfiles/executable_dot_xinitrc;
    executable = true;
  };

  # PERSONAL SCRIPTS
  home.file.".sources" = {
    source = ../../sources;
    executable = true;
    recursive = true;
  };

}
