{ config, pkgs, libs, ... }:

{
  # QUTEBROWSER
  home.file = {
    ".config/qutebrowser/config.py" = {
      source = ../dotfiles/dot_config/private_qutebrowser/config.py;
    };
    ".config/qutebrowser/nord-qutebrowser.py" = {
      source = ../dotfiles/dot_config/private_qutebrowser/nord-qutebrowser.py;
    };
    ".config/qutebrowser/qsettings/QtProject.conf" = {
      source = ../dotfiles/dot_config/private_qutebrowser/qsettings/QtProject.conf;
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
  xdg.configFile."dunst/dunstrc".source = ../dotfiles/dot_config/dunst/dunstrc;
  xdg.configFile."dunst/critical.png".source = ../dotfiles/dot_config/dunst/critical.png;
  xdg.configFile."dunst/normal.png".source = ../dotfiles/dot_config/dunst/normal.png;
  xdg.configFile."dunst/low.png".source = ../dotfiles/dot_config/dunst/low.png;

  # LF
  xdg.configFile."lf/lfrc".source = ../dotfiles/dot_config/lf/lfrc;

  # NEOFETCH
  xdg.configFile."noefetch/config.conf".source = ../dotfiles/dot_config/neofetch/config.conf;

  # PICOM 
  xdg.configFile."picom/picom.conf".source = ../dotfiles/dot_config/picom/picom.conf;

  # STARSHIP
  xdg.configFile."starship.toml".source = ../dotfiles/dot_config/starship.toml;

  # Xresources
  home.file.".Xresources" = {
    source = ../dotfiles/dot_Xresources;
  };

  # xinitrc
  home.file.".xinitrc" = {
    source = ../dotfiles/executable_dot_xinitrc;
    executable = true;
  };

  # PERSONAL SCRIPTS
  home.file.".sources" = {
    source = ../sources;
    executable = true;
    recursive = true;
  };

  # GIT CONFIG
  home.file.".gitconfig" = {
    source = ../dotfiles/dot_gitconfig;
  };

  # GITUI
  xdg.configFile."gitui/key_config.ron".source = ../dotfiles/dot_config/gitui/key_config.ron;
  xdg.configFile."gitui/theme.ron".source = ../dotfiles/dot_config/gitui/theme.ron;
}
