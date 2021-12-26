{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    slack
    unstable.tdesktop
    teams
    zoom-us
    discord
  ];
}
