{ config, pkgs, libs, ... }:

{
  # QUTEBROWSER
  home.file = {
    ".config/qutebrowser/config.py" = {
      source = ../dotfiles/qutebrowser/config.py;
    };
    ".config/qutebrowser/nord-qutebrowser.py" = {
      source = ../dotfiles/qutebrowser/nord-qutebrowser.py;
    };
    ".config/qutebrowser/qsettings/QtProject.conf" = {
      source = ../dotfiles/qutebrowser/qsettings/QtProject.conf;
    };
    # ".local/share/qutebrowser/userscripts/rofiQutebrowser" = {
    #   source = ../dotfiles/dot_local/share/private_qutebrowser/userscripts/executable_rofiQutebrowser;
    #   executable = true;
    # };
    ".config/qutebrowser/browser-home" = {
      source = ../browser-home;
      recursive = true;
    };
  };

  # DUNST
  xdg.configFile."dunst/dunstrc".source = ../dotfiles/dunst/dunstrc;
  xdg.configFile."dunst/critical.png".source = ../dotfiles/dunst/critical.png;
  xdg.configFile."dunst/normal.png".source = ../dotfiles/dunst/normal.png;
  xdg.configFile."dunst/low.png".source = ../dotfiles/dunst/low.png;

  # xinitrc
  home.file.".xinitrc" = {
    source = ../dotfiles/.xinitrc;
    executable = true;
  };

  # PERSONAL SCRIPTS
  home.file.".sources" = {
    source = ../sources;
    executable = true;
    recursive = true;
  };

  home.file.".xmonad/pacwall.png".source = ../dotfiles/xmonad/wallpaper.png;

  # SSH CONFIG
  home.file.".ssh/config" = {
    source = ../dotfiles/ssh/config;
  };

  # lua formatter
  xdg.configFile."luaformatter/config.yaml".source = ../dotfiles/lua_format.yaml;
}
