{ config, pkgs, libs, ... }:

{
  # BSPWM
  xdg.configFile."bspwm/bspwmrc".source = ../../dotfiles/dot_config/bspwm/executable_bspwmrc;
  xdg.configFile."bspwm/pacwall.png".source = ../../dotfiles/dot_config/bspwm/pacwall.png;

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
  xdg.configFile."noefetch/config.conf".source = ../../dotfiles/dot_config/noefetch/config.conf;

  # ROFI
  xdg.configFile."rofi/config.rasi".source = ../../dotfiles/dot_config/rofi/config.rasi;

  # STARSHIP
  xdg.configFile."starship.toml".source = ../../dotfiles/dot_config/starship.toml;

  # SXHKD
  xdg.configFile."sxhkd/sxhkdrc".source = ../../dotfiles/dot_config/sxhkd/sxhkdrc;
}
