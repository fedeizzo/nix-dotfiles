{ config, pkgs, libs, ... }:

{
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
}
