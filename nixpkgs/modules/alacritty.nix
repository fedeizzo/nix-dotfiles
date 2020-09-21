{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    alacritty
  ];
  xdg.configFile."alacritty/alacritty.yml".source = ../dotfiles/dot_config/alacritty/alacritty.yml;
}
