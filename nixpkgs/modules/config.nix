{ config, pkgs, libs, ... }:

{
  # DUNST
  xdg.configFile."dunst/dunstrc".source = ../dotfiles/dunst/dunstrc;
  xdg.configFile."dunst/critical.png".source = ../dotfiles/dunst/critical.png;
  xdg.configFile."dunst/normal.png".source = ../dotfiles/dunst/normal.png;
  xdg.configFile."dunst/low.png".source = ../dotfiles/dunst/low.png;

  # PERSONAL SCRIPTS
  home.file.".sources" = {
    source = ../sources;
    executable = true;
    recursive = true;
  };

  # SSH CONFIG
  home.file.".ssh/config" = {
    source = ../dotfiles/ssh/config;
  };

  # lua formatter
  xdg.configFile."luaformatter/config.yaml".source = ../dotfiles/lua_format.yaml;
}
