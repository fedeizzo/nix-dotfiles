{ config, pkgs, libs, ... }:

{
  home.file.".sources" = {
    source = ../sources;
    executable = true;
    recursive = true;
  };

  home.file.".ssh/config" = {
    source = ../dotfiles/ssh/config;
  };

  xdg.configFile."networkmanager-dmenu/config.ini" = {
    source = ../dotfiles/networkmanager-dmenu.ini;
  };

  nix.registry = {
    fedeizzo = {
      from = {
        id = "fedeizzo";
        type = "indirect";
      };
      to = {
        owner = "fedeizzo";
        repo = "nix-dotfiles";
        type = "github";
      };
    };
  };
}
