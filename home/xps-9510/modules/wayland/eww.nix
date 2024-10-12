{ pkgs, ... }:

{
  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland;
    configDir = ./eww;
  };
  home.file.".pam_environment" = {
    text = "XDG_CURRENT_DESKTOP_DEFAULT=Hyprland";
  };
}
