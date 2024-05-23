{ pkgs, ... }:

{
  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland;
    configDir = ../../dotfiles/eww;
  };
}
