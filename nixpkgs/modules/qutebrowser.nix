{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    qutebrowser
  ];
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
    ".config/qutebrowser/browser-home" = {
      source = ../browser-home;
      recursive = true;
    };
  };
}
