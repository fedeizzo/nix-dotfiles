{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    slack
    tdesktop
    teams
    zoom-us
    discord
  ];
}
